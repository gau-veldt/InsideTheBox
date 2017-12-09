----------------------------------------------------------------------------------
--
--  Takes all the VHDL bits and makes a 6510 (6502) out of them
--
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity chip6502 is
port (
    a       :   out     std_logic_vector(15 downto 0);
    di      :   in      std_logic_vector(7 downto 0);
    do      :   out     std_logic_vector(7 downto 0);
    pi      :   in      std_logic_vector(7 downto 0);
    po      :   out     std_logic_vector(7 downto 0);
    r1w0    :   out     std_logic;
    sync    :   out     std_logic;
    nmi0    :   in      std_logic;
    irq0    :   in      std_logic;
    so0     :   in      std_logic;        
    rdy     :   in      std_logic;
    res0    :   in      std_logic;
    ph4Xin  :   in      std_logic;  -- clock input
    ph0     :   out     std_logic;
    ph1     :   out     std_logic;  -- clock on high edge
    ph2     :   out     std_logic   -- clock on low edge
);
end chip6502;

architecture interaction of chip6502 is

subtype slv2        is std_logic_vector(1 downto 0);
subtype u8          is unsigned(7 downto 0);
subtype byte        is std_logic_vector(7 downto 0);
subtype u16         is unsigned(15 downto 0);
subtype word        is std_logic_vector(15 downto 0);

constant v_nmi_l : word := x"FFFA";
constant v_nmi_h : word := x"FFFB";
constant v_res_l : word := x"FFFC";
constant v_res_h : word := x"FFFD";
constant v_irq_l : word := x"FFFE";
constant v_irq_h : word := x"FFFF";

component clockgen
port (
    ph4Xin  :   in  std_logic;
    ph0     :   out std_logic;
    ph1     :   out std_logic;
    ph2     :   out std_logic;
    stg     :   out slv2;
    res0    :   in  std_logic
);
end component;

subtype clkstg_t is std_logic_vector(1 downto 0);
signal clkStg : clkstg_t;
signal iph0 : std_logic;
signal iph1 : std_logic;
signal iph2 : std_logic;
constant sysclk_PH2_p : clkstg_t := "00"; 
constant sysclk_PH2_m : clkstg_t := "01"; 
constant sysclk_PH1_p : clkstg_t := "10"; 
constant sysclk_PH1_m : clkstg_t := "11"; 

component ioport8bit is
port (
    ce      : in    std_logic;
    clk     : in    std_logic;
    res0    : in    std_logic;
    r1w0    : in    std_logic;
    a       : in    std_logic;
    din     : in    byte;
    dout    : out   byte;
    ioi     : in    byte;
    ioo     : out   byte
);
end component;
signal  io_o    : byte;
signal  io_i    : byte;
signal  io_ce   : std_logic;
signal  io_clk  : std_logic;

component alu_8bit
	port (
    a_in        :    in byte;
    b_in        :    in byte;
    c_in        :    in std_logic;
    d_in        :    in std_logic;                            -- the dreaded BCD mode
    op_in        :    in unsigned(2 downto 0);

    n_out        :    out std_logic;
    v_out        :    out std_logic;
    z_out        :    out std_logic;
    c_out        :    out std_logic;
    r_out        :    out byte
);
end component;
signal ALUcin   : std_logic;
signal ALUdin   : std_logic;
signal ALUain   : byte;
signal ALUbin   : byte;
signal ALUop    : unsigned(2 downto 0);
signal ALUrout  : byte;
signal ALUnout  : std_logic;
signal ALUvout  : std_logic;
signal ALUzout  : std_logic;
signal ALUcout  : std_logic;

signal regbus   : byte;
signal outval   : byte;
signal abus     : word;
signal DBen     : std_logic := '1';
signal DBrw     : std_logic := '1';
signal dbRE     : std_logic;
signal dbWE     : std_logic;
signal nDBen    : std_logic;
signal aen0     : std_logic := '1';
signal aen1     : std_logic;
alias abus_off  is aen0;

subtype seqType is byte;
function countSeq(src : seqType) return seqType is
variable v : unsigned(7 downto 0);
begin
    v := unsigned(src);
    v := v + 1;
    return seqType(v);
end countSeq;
signal seq : seqType := x"00";

subtype dbctl_t is std_logic_vector(2 downto 0);
signal DB_ctl       : dbctl_t := "011";
alias dbctl_sync : std_logic is DB_ctl(2);
alias dbctl_off : std_logic is DB_ctl(1);
alias dbctl_r1w0 : std_logic is DB_ctl(0);

subtype aop_t is std_logic_vector(2 downto 0);
signal aop : aop_t := "000";
constant aop_add : aop_t := "000";
constant aop_and : aop_t := "001";
constant aop_or  : aop_t := "010";
constant aop_xor : aop_t := "011";
constant aop_lsl : aop_t := "100";
constant aop_lsr : aop_t := "101";
constant aop_rol : aop_t := "110";
constant aop_ror : aop_t := "111";

signal alu_bin_mode : slv2;
constant bin_reg    : slv2 := "00";
constant bin_set    : slv2 := "01";
constant bin_clr    : slv2 := "10";
constant bin_ireg   : slv2 := "11";

signal alu_cin_mode : slv2;
constant cin_psw : slv2 := "00";
constant cin_set : slv2 := "01";
constant cin_clr : slv2 := "10";
constant cin_aux : slv2 := "11";

signal alu_din_mode : std_logic;
constant din_clr : std_logic := '0';
constant din_psw : std_logic := '1';

--signal alu_bin_reg : byte;
alias alu_bin_reg : byte is ALUbin;
signal alu_bin_tie : byte;

type stage_t is (
    stg_reset,
    stg_fetch,
    stg_sub_incpc,          -- pc++
    stg_sub_imm,            -- mem=pc++
    stg_sub_abs,            -- meml=[pc++], memh=[pc++]
    stg_sub_absx,           -- meml=[X+(PC++)], memh=[C+(PC++)]
    stg_sub_absy,           -- meml=[Y+(PC++)], memh=[C+(PC++)]
    stg_sub_zp,             -- meml=[PC++], memh=[0]
    stg_sub_zpx,            -- meml=[X+(PC++)], memh=[0]
    stg_sub_zpy,            -- meml=[Y+(PC++)], memh=[0]
    stg_sub_indx,           -- buf2l=[X+(PC++)], buf2h=[0]. mem=[buf2].w
    stg_sub_indy,           -- buf2l=[PC++], buf2h=[0], mem=[Y+buf2].w
    stg_mem2buf,            -- BUF=[mem]
    stg_mem2a,              -- A=[mem]
    stg_aCMPmem,            -- NZC=A-[mem]
    stg_aADDmem,            -- NVZC,A=A+[mem]
    stg_aSUBmem,            -- NVZC,A=A-[mem]
    stg_aORmem,             -- A=A|[mem]
    stg_aXORmem,            -- A=A^[mem]
    stg_aANDmem,            -- A=A&mem
    stg_xCMPmem,            -- NZC=X-[mem]
    stg_yCMPmem,            -- NZC=Y-[mem]
    stg_mem2x,              -- X=[mem]
    stg_mem2y,              -- Y=[mem]
    stg_a2mem,              -- [mem]=A
    stg_x2mem,              -- [mem]=X
    stg_y2mem,              -- [mem]=Y
    stg_ASLmem,             -- ***TODO*** C <-- [7][mem][0] <-- 0
    stg_ROLmem,             -- ***TODO*** C <-- [7][mem][0] <-- C
    stg_LSRmem,             -- ***TODO*** 0 --> [7][mem][0] --> C
    stg_RORmem,             -- ***TODO*** C --> [7][mem][0] --> C
    stg_INCmem,             -- NZ=++[mem]
    stg_DECmem,             -- NZ=--[mem]
    stg_BITmem,             -- Z=[mem]&A, NV=[mem][7:6]
    stg_BRK,                -- ***TODO*** ++PC, B=1, raise (unmaskable) IRQ
    stg_IRQ,                -- [SP--]=PCH, [SP--]=PCL, [SP--]=PSW, I=1, PCL=[v_irq_l], PCH=[v_irq_h]
    stg_NMI,                -- [SP--]=PCH, [SP--]=PCL, [SP--]=PSW, I=1, PCL=[v_nmi_l], PCH=[v_nmi_h]
    stg_CLC,
    stg_SEC,
    stg_CLI,
    stg_SEI,
    stg_CLV,
    stg_CLD,
    stg_SED,
    stg_TXS,
    stg_TSX,
    stg_PHA,
    stg_PLA,
    stg_PHP,
    stg_PLP,
    stg_TAX,
    stg_TXA,
    stg_TAY,
    stg_TYA,
    stg_DEX,
    stg_DEY,
    stg_INX,
    stg_INY,
    stg_JMP_abs,
    stg_JMP_ind,
    stg_reljmp,             -- take branch
    stg_BCC,
    stg_BCS,
    stg_BNE,
    stg_BEQ,
    stg_BPL,
    stg_BMI,
    stg_BVC,
    stg_BVS,
    stg_ASL_a,
    stg_LSR_a,
    stg_ROL_a,
    stg_ROR_a,
    stg_RTS,
    stg_RTI,
    stg_JSR,
    stg_tail
);
signal seq_stage : stage_t := stg_reset;
signal ret_stage : stage_t := stg_fetch;    -- return stage for sub stage

signal ir       : byte := x"00";
signal reg_a    : byte := x"00";
signal reg_x    : byte := x"00";
signal reg_y    : byte := x"00";
signal reg_pc   : word := x"0000";
signal reg_sp   : byte := x"00";
signal reg_p    : byte := x"00";
alias  psw_n    is reg_p(7);
alias  psw_v    is reg_p(6);
alias  psw_b    is reg_p(4);
alias  psw_d    is reg_p(3);
alias  psw_i    is reg_p(2);
alias  psw_z    is reg_p(1);
alias  psw_c    is reg_p(0);
signal buf_data : byte := x"00";
signal buf_addr : word := x"0000";
signal buf2 : word := x"0000";
alias reg_pcl : byte is reg_pc(7 downto 0);
alias reg_pch : byte is reg_pc(15 downto 8);
alias buf_addr_l : byte is buf_addr(7 downto 0);
alias buf_addr_h : byte is buf_addr(15 downto 8);
alias buf2l : byte is buf2(7 downto 0);
alias buf2h : byte is buf2(15 downto 8);

signal private_c    : std_logic;

signal NMI_last     : std_logic;

function isZero(src: byte) return std_logic is
begin
    return ((src(0) nor src(1)) and (src(2) nor src(3))) and
           ((src(4) nor src(5)) and (src(6) nor src(7)));
end isZero;

function dec(arg : byte) return byte is
variable argu : unsigned(8 downto 0);
begin
    argu := ('0' & u8(arg)) - "000000001";
    return byte(argu(7 downto 0));
end dec;

function inc(arg : byte) return byte is
variable argu : unsigned(8 downto 0);
begin
    argu := ('0' & u8(arg)) + "000000001";
    return byte(argu(7 downto 0));
end inc;

function inc16(arg : word) return word is
variable argu : unsigned(16 downto 0);
begin
    argu := ('0' & u16(arg)) + ('0' & x"01");
    return word(argu(15 downto 0));
end inc16;

function sgn(arg : byte) return byte is
begin
    return arg(7) & arg(7) & arg(7) & arg(7) &
           arg(7) & arg(7) & arg(7) & arg(7);
end sgn;

function getb(arg : byte; bp : integer) return std_logic is
begin
    return arg(bp);
end getb;

begin

    clock: clockgen port map(
        ph4Xin => ph4Xin,
        ph0 => iph0,    
        ph1 => iph1,    
        ph2 => iph2,
        stg => clkStg,    
        res0 => res0
    );
    ph0 <= iph0;
    ph1 <= iph1;
    ph2 <= iph2;

    io8bit: ioport8bit port map(
        ce => io_ce,
        clk => io_clk,
        res0 => res0,
        r1w0 => dbRW,
        a => abus(0),
        din => io_i,
        dout => io_o,
        ioi => pi,
        ioo => po
    );
    io_clk <= (not clkStg(1)) and clkStg(0);
    io_ce <= not (  (((abus(15) or abus(14)) or (abus(13) or abus(12)))  or
                     ((abus(11) or abus(10)) or (abus( 9) or abus( 8)))) or
                    (((abus( 7) or abus( 6)) or (abus( 5) or abus( 4)))  or
                     ((abus( 3) or abus( 2)) or abus( 1)              ))  );
    io_i <= regbus;

    alunit: alu_8bit port map(
        a_in => ALUain,
        b_in => ALU_bin_tie,
        c_in => ALUcin,
        d_in => ALUdin,
        op_in => ALUop,
        n_out => ALUnout,
        v_out => ALUvout,
        z_out => ALUzout,
        c_out => ALUcout,
        r_out => ALUrout
    );
    
    alu_cin_mux: process(alu_cin_mode,psw_c,private_c) is
    begin
        case alu_cin_mode is
            when cin_set  => ALUcin <= '1';
            when cin_clr  => ALUcin <= '0';
            when cin_aux  => ALUcin <= private_c;
            when others     => ALUcin <= psw_c;
        end case;
    end process alu_cin_mux;

    alu_din_mux: process(alu_din_mode,psw_d) is
    begin
        case alu_din_mode is
            when din_clr    => ALUdin <= '0';
            when others     => ALUdin <= psw_d;
        end case;
    end process alu_din_mux;

    alu_bin_mux: process(alu_bin_mode,alu_bin_reg) is
    begin
        case alu_bin_mode is
            when bin_clr  => alu_bin_tie <= "00000000";
            when bin_set  => alu_bin_tie <= "11111111";
            when bin_ireg => alu_bin_tie <= not alu_bin_reg;
            when others     => alu_bin_tie <= alu_bin_reg;
        end case;
    end process alu_bin_mux;

    sync <= DB_ctl(2);
    DBen <= DB_ctl(1);
    DBrw <= DB_ctl(0);
    nDBen <= not DBen;
    DBre <= DBrw;
    DBwe <= not DBrw;
    aen1 <= not aen0;
    r1w0 <= DBrw;
    ALUop <= unsigned(aop);

    -- Allow connection of data bus as output during write operations or disconnected
    -- (high-Z) otherwise allowing other devices to use data bus while the CPU is halted.
    db_ogate: process(nDBen,DBwe,outval) iS
    begin
        if ((nDBen and DBwe) = '1') then
            if (io_ce='1') then
                do <= io_o;
            else
                do <= outval;
            end if;
        else
            do <= "ZZZZZZZZ";
        end if;
    end process db_ogate;

    -- Allow connection of data bus as input during read operations or disconnected
    -- (high-Z) otherwise allowing other devices to use data bus while the CPU is halted.
    db_igate: process(nDBen,DBre,di) is
    begin
        if ((nDBen and DBre) = '1') then
            regbus <= di;
        else
            regbus <= "ZZZZZZZZ";
        end if;
    end process db_igate;

    addr_gate: process(aen1,abus) is
    begin
        if (aen1='1') then
            a <= abus;
        else
            a <= "ZZZZZZZZZZZZZZZZ";
        end if;
    end process addr_gate;

    main_proc: process(res0,ph4Xin) is
    variable doNMI : std_logic;
    variable tmp8 : byte;
    begin
        -- Status register stuff
        reg_p(5) <= '0';
        if (so0 = '0') then
            psw_V <= '1';
        end if;

        if (res0 = '0') then
            seq <= x"00";
            seq_stage <= stg_reset;
            ret_stage <= stg_reset;
            dbctl_r1w0 <= '1';
            dbctl_off <= '1';
            dbctl_sync <= '0';
            abus_off <= '1';
            doNMI := '0';
            
        elsif (rising_edge(ph4Xin)) then
 
            -- allows sensing NMI on any clock
            -- (but won't trigger until epilogue)
            doNMI := doNMI or (NMI_last and (not nmi0));    -- only on transition to '0'
            NMI_last <= nmi0;                               -- saves the state read

            -- reset stage
            if (seq_stage = stg_reset) then
                if ((not (clkStg = sysclk_PH2_m)) and seq=x"00") then
                else
                    -- we enter here at seq x00 on PH1+
                    case seq is
                        when x"00" => seq <= countSeq(seq);     -- PH1+: put RES vector L on abus
                            dbctl_off <= '0';
                            dbctl_r1w0 <= '1';
                            abus_off <= '0';
                            abus <= v_res_l;
                        when x"01" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                        when x"02" => seq <= countSeq(seq);     -- PH2+: pass
                        when x"03" =>                           -- PH2-: (valid data) write to PCL
                            reg_pcl <= regbus;
                            seq <= countSeq(seq);
                        when x"04" =>                           -- PH1+: put RES vector H on abus
                            abus <= v_res_h;
                            seq <= countSeq(seq);
                        when x"05" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                        when x"06" => seq <= countSeq(seq);     -- PH2+: pass
                            seq <= countSeq(seq);
                        when x"07" =>                           -- PH2-: (valid data) write to PCH
                            reg_pch <= regbus;
                            seq_stage <= stg_fetch;             --       change to instruction decode
                            seq <= x"00";
                        when others => null;
                    end case;
                end if;
            end if;

            -- instruction fetch/decode stage
            if (seq_stage = stg_fetch) then
                case seq is
                    when x"00" =>                               -- PH1+: sync on, PC to abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus on for read
                        dbctl_sync <= '1';                      --       sync on for instruction decode
                        abus_off <= '0';
                        abus <= reg_pc;                         --       PC on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data) get instruction
                        ir <= regbus;
                        seq <= countSeq(seq);
                    when x"04" =>                               -- PH1+: sync off
                        dbctl_sync <= '0';
                        ALUain <= reg_pcl;
                        alu_cin_mode <= cin_set;
                        alu_bin_mode <= bin_clr;
                        alu_din_mode <= din_clr;
                        aop <= aop_add;
                        seq <= countSeq(seq);
                    when x"05" =>                               -- PH1-: store PCL=1+PCL
                        reg_pcl <= ALUrout;
                        private_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"06" =>                               -- PH2+:
                        ALUain <= reg_pch;
                        alu_cin_mode <= cin_aux;                --       apply carry from PCL+1
                        seq <= countSeq(seq);
                    when x"07" =>                               -- PH2-:
                        reg_pch <= ALUrout;                     --       store PCH=PCH+C
                        if (rdy = '1') then
                            abus_off <= '0';
                            dbctl_off <= '0';
                            seq <= x"00";
                            case ir is

                                when x"4C" =>
                                    seq_stage <= stg_JMP_abs;       -- JMP abs
                                when x"6C" =>
                                    seq_stage <= stg_JMP_ind;       -- JMP ind
                            
                                when x"18" =>
                                    seq_stage <= stg_CLC;           -- CLC
                                when x"38" =>
                                    seq_stage <= stg_SEC;           -- SEC
                                when x"58" =>
                                    seq_stage <= stg_CLI;           -- CLI
                                when x"78" =>
                                    seq_stage <= stg_SEI;           -- SEI
                                when x"B8" =>
                                    seq_stage <= stg_CLV;           -- CLV
                                when x"D8" =>
                                    seq_stage <= stg_CLD;           -- CLD
                                when x"F8" =>
                                    seq_stage <= stg_SED;           -- SED
    
                                when x"A9" =>                       -- LDA
                                    ret_stage <= stg_mem2a;         --  imm
                                    seq_stage <= stg_sub_imm;
                                when x"AD" =>                       --  abs
                                    ret_stage <= stg_mem2a;
                                    seq_stage <= stg_sub_abs;
                                when x"BD" =>                       --  abs+x
                                    ret_stage <= stg_mem2a;
                                    seq_stage <= stg_sub_absx;
                                when x"B9" =>                       --  abs+y
                                    ret_stage <= stg_mem2a;
                                    seq_stage <= stg_sub_absy;
                                when x"A5" =>                       --  zp
                                    ret_stage <= stg_mem2a;
                                    seq_stage <= stg_sub_zp;
                                when x"B5" =>                       --  zp+x
                                    ret_stage <= stg_mem2a;
                                    seq_stage <= stg_sub_zpx;
                                when x"A1" =>                       --  indirect,X
                                    ret_stage <= stg_mem2a;
                                    seq_stage <= stg_sub_indx;
                                when x"B1" =>                       --  indirect,Y
                                    ret_stage <= stg_mem2a;
                                    seq_stage <= stg_sub_indy;
    
                                when x"A2" =>                       -- LDX
                                    ret_stage <= stg_mem2x;         --  imm
                                    seq_stage <= stg_sub_imm;
                                when x"AE" =>                       --  abs
                                    ret_stage <= stg_mem2x;
                                    seq_stage <= stg_sub_abs;
                                when x"BE" =>                       --  abs+y
                                    ret_stage <= stg_mem2x;
                                    seq_stage <= stg_sub_absy;
                                when x"A6" =>                       --  zp
                                    ret_stage <= stg_mem2x;
                                    seq_stage <= stg_sub_zp;
                                when x"B6" =>                       --  zp+y
                                    ret_stage <= stg_mem2x;
                                    seq_stage <= stg_sub_zpy;

                                when x"A0" =>                       -- LDY
                                    ret_stage <= stg_mem2y;         --  imm
                                    seq_stage <= stg_sub_imm;
                                when x"AC" =>                       --  abs
                                    ret_stage <= stg_mem2y;
                                    seq_stage <= stg_sub_abs;
                                when x"BC" =>                       --  abs+x
                                    ret_stage <= stg_mem2y;
                                    seq_stage <= stg_sub_absx;
                                when x"A4" =>                       --  zp
                                    ret_stage <= stg_mem2y;
                                    seq_stage <= stg_sub_zp;
                                when x"B4" =>                       --  zp+x
                                    ret_stage <= stg_mem2y;
                                    seq_stage <= stg_sub_zpx;

                                when x"8D" =>                       -- STA
                                    ret_stage <= stg_a2mem;         --  abs
                                    seq_stage <= stg_sub_abs;
                                when x"9D" =>                       --  abs+x
                                    ret_stage <= stg_a2mem;
                                    seq_stage <= stg_sub_absx;
                                when x"99" =>                       --  abs+y
                                    ret_stage <= stg_a2mem;
                                    seq_stage <= stg_sub_absy;
                                when x"85" =>                       --  zp
                                    ret_stage <= stg_a2mem;
                                    seq_stage <= stg_sub_zp;
                                when x"95" =>                       --  zp+x
                                    ret_stage <= stg_a2mem;
                                    seq_stage <= stg_sub_zpx;
                                when x"81" =>                       --  indirect,X
                                    ret_stage <= stg_a2mem;
                                    seq_stage <= stg_sub_indx;
                                when x"91" =>                       --  indirect,Y
                                    ret_stage <= stg_a2mem;
                                    seq_stage <= stg_sub_indy;
    
                                when x"8E" =>                       -- STX
                                    ret_stage <= stg_x2mem;         --  abs
                                    seq_stage <= stg_sub_abs;
                                when x"86" =>                       --  zp
                                    ret_stage <= stg_x2mem;
                                    seq_stage <= stg_sub_zp;
                                when x"96" =>                       --  zp+y
                                    ret_stage <= stg_x2mem;
                                    seq_stage <= stg_sub_zpy;
        
                                when x"8C" =>                       -- STY
                                    ret_stage <= stg_y2mem;         --  abs
                                    seq_stage <= stg_sub_abs;
                                when x"84" =>                       --  zp
                                    ret_stage <= stg_y2mem;
                                    seq_stage <= stg_sub_zp;
                                when x"94" =>                       --  zp+x
                                    ret_stage <= stg_y2mem;
                                    seq_stage <= stg_sub_zpx;

                                when x"09" =>                       -- ORA A | mem
                                    ret_stage <= stg_aORmem;        --  imm
                                    seq_stage <= stg_sub_imm;
                                when x"0D" =>                       --  abs
                                    ret_stage <= stg_aORmem;
                                    seq_stage <= stg_sub_abs;
                                when x"1D" =>                       --  abs+x
                                    ret_stage <= stg_aORmem;
                                    seq_stage <= stg_sub_absx;
                                when x"19" =>                       --  abs+y
                                    ret_stage <= stg_aORmem;
                                    seq_stage <= stg_sub_absy;
                                when x"05" =>                       --  zp
                                    ret_stage <= stg_aORmem;
                                    seq_stage <= stg_sub_zp;
                                when x"15" =>                       --  zp+x
                                    ret_stage <= stg_aORmem;
                                    seq_stage <= stg_sub_zpx;
                                when x"01" =>                       --  indirect,X
                                    ret_stage <= stg_aORmem;
                                    seq_stage <= stg_sub_indx;
                                when x"11" =>                       --  indirect,Y
                                    ret_stage <= stg_aORmem;
                                    seq_stage <= stg_sub_indy;

                                when x"29" =>                       -- AND A & mem
                                    ret_stage <= stg_aANDmem;       --  imm
                                    seq_stage <= stg_sub_imm;
                                when x"2D" =>                       --  abs
                                    ret_stage <= stg_aANDmem;
                                    seq_stage <= stg_sub_abs;
                                when x"3D" =>                       --  abs+x
                                    ret_stage <= stg_aANDmem;
                                    seq_stage <= stg_sub_absx;
                                when x"39" =>                       --  abs+y
                                    ret_stage <= stg_aANDmem;
                                    seq_stage <= stg_sub_absy;
                                when x"25" =>                       --  zp
                                    ret_stage <= stg_aANDmem;
                                    seq_stage <= stg_sub_zp;
                                when x"35" =>                       --  zp+x
                                    ret_stage <= stg_aANDmem;
                                    seq_stage <= stg_sub_zpx;
                                when x"21" =>                       --  indirect,X
                                    ret_stage <= stg_aANDmem;
                                    seq_stage <= stg_sub_indx;
                                when x"31" =>                       --  indirect,Y
                                    ret_stage <= stg_aANDmem;
                                    seq_stage <= stg_sub_indy;

                                when x"24" =>                       -- BIT Z=A&M, NV=[MEM][7:6]
                                    ret_stage <= stg_BITmem;        --  zp
                                    seq_stage <= stg_sub_zp;
                                when x"2C" =>                       --  abs
                                    ret_stage <= stg_BITmem;
                                    seq_stage <= stg_sub_abs;

                                when x"49" =>                       -- EOR A ^ mem
                                    ret_stage <= stg_aXORmem;       --  imm
                                    seq_stage <= stg_sub_imm;
                                when x"4D" =>                       --  abs
                                    ret_stage <= stg_aXORmem;
                                    seq_stage <= stg_sub_abs;
                                when x"5D" =>                       --  abs+x
                                    ret_stage <= stg_aXORmem;
                                    seq_stage <= stg_sub_absx;
                                when x"59" =>                       --  abs+y
                                    ret_stage <= stg_aXORmem;
                                    seq_stage <= stg_sub_absy;
                                when x"45" =>                       --  zp
                                    ret_stage <= stg_aXORmem;
                                    seq_stage <= stg_sub_zp;
                                when x"55" =>                       --  zp+x
                                    ret_stage <= stg_aXORmem;
                                    seq_stage <= stg_sub_zpx;
                                when x"41" =>                       --  indirect,X
                                    ret_stage <= stg_aXORmem;
                                    seq_stage <= stg_sub_indx;
                                when x"51" =>                       --  indirect,Y
                                    ret_stage <= stg_aXORmem;
                                    seq_stage <= stg_sub_indy;

                                when x"69" =>                       -- ADC C + A + mem
                                    ret_stage <= stg_aADDmem;       --  imm
                                    seq_stage <= stg_sub_imm;
                                when x"6D" =>                       --  abs
                                    ret_stage <= stg_aADDmem;
                                    seq_stage <= stg_sub_abs;
                                when x"7D" =>                       --  abs+x
                                    ret_stage <= stg_aADDmem;
                                    seq_stage <= stg_sub_absx;
                                when x"79" =>                       --  abs+y
                                    ret_stage <= stg_aADDmem;
                                    seq_stage <= stg_sub_absy;
                                when x"65" =>                       --  zp
                                    ret_stage <= stg_aADDmem;
                                    seq_stage <= stg_sub_zp;
                                when x"75" =>                       --  zp+x
                                    ret_stage <= stg_aADDmem;
                                    seq_stage <= stg_sub_zpx;
                                when x"61" =>                       --  indirect,X
                                    ret_stage <= stg_aADDmem;
                                    seq_stage <= stg_sub_indx;
                                when x"71" =>                       --  indirect,Y
                                    ret_stage <= stg_aADDmem;
                                    seq_stage <= stg_sub_indy;

                                when x"C9" =>                       -- CMP NZC = A - mem
                                    ret_stage <= stg_aCMPmem;       --  imm
                                    seq_stage <= stg_sub_imm;
                                when x"CD" =>                       --  abs
                                    ret_stage <= stg_aCMPmem;
                                    seq_stage <= stg_sub_abs;
                                when x"DD" =>                       --  abs+x
                                    ret_stage <= stg_aCMPmem;
                                    seq_stage <= stg_sub_absx;
                                when x"D9" =>                       --  abs+y
                                    ret_stage <= stg_aCMPmem;
                                    seq_stage <= stg_sub_absy;
                                when x"C5" =>                       --  zp
                                    ret_stage <= stg_aCMPmem;
                                    seq_stage <= stg_sub_zp;
                                when x"D5" =>                       --  zp+x
                                    ret_stage <= stg_aCMPmem;
                                    seq_stage <= stg_sub_zpx;
                                when x"C1" =>                       --  indirect,X
                                    ret_stage <= stg_aCMPmem;
                                    seq_stage <= stg_sub_indx;
                                when x"D1" =>                       --  indirect,Y
                                    ret_stage <= stg_aCMPmem;
                                    seq_stage <= stg_sub_indy;

                                when x"C0" =>                       -- CPY NZC = Y - mem
                                    ret_stage <= stg_yCMPmem;       --  imm
                                    seq_stage <= stg_sub_imm;
                                when x"C4" =>                       --  zp
                                    ret_stage <= stg_yCMPmem;
                                    seq_stage <= stg_sub_zp;
                                when x"CC" =>                       --  abs
                                    ret_stage <= stg_yCMPmem;
                                    seq_stage <= stg_sub_abs;

                                when x"E0" =>                       -- CPX NZC = X - mem
                                    ret_stage <= stg_xCMPmem;       --  imm
                                    seq_stage <= stg_sub_imm;
                                when x"E4" =>                       --  zp
                                    ret_stage <= stg_xCMPmem;
                                    seq_stage <= stg_sub_zp;
                                when x"EC" =>                       --  abs
                                    ret_stage <= stg_xCMPmem;
                                    seq_stage <= stg_sub_abs;

                                when x"E9" =>                       -- SBC A + C - (mem+1)
                                    ret_stage <= stg_aSUBmem;       --  imm
                                    seq_stage <= stg_sub_imm;
                                when x"ED" =>                       --  abs
                                    ret_stage <= stg_aSUBmem;
                                    seq_stage <= stg_sub_abs;
                                when x"FD" =>                       --  abs+x
                                    ret_stage <= stg_aSUBmem;
                                    seq_stage <= stg_sub_absx;
                                when x"F9" =>                       --  abs+y
                                    ret_stage <= stg_aSUBmem;
                                    seq_stage <= stg_sub_absy;
                                when x"E5" =>                       --  zp
                                    ret_stage <= stg_aSUBmem;
                                    seq_stage <= stg_sub_zp;
                                when x"F5" =>                       --  zp+x
                                    ret_stage <= stg_aSUBmem;
                                    seq_stage <= stg_sub_zpx;
                                when x"E1" =>                       --  indirect,X
                                    ret_stage <= stg_aSUBmem;
                                    seq_stage <= stg_sub_indx;
                                when x"F1" =>                       --  indirect,Y
                                    ret_stage <= stg_aSUBmem;
                                    seq_stage <= stg_sub_indy;

                                when x"9A" =>                       -- TXS sp=x
                                    seq_stage <= stg_TXS;
                                when x"BA" =>                       -- TSX x=sp
                                    seq_stage <= stg_TSX;
                                when x"48" =>                       -- PHA [sp--]=a
                                    seq_stage <= stg_PHA;
                                when x"68" =>                       -- PLA a=[++sp]
                                    seq_stage <= stg_PLA;
                                when x"08" =>                       -- PHP [sp--]=P
                                    seq_stage <= stg_PHP;
                                when x"28" =>                       -- PLP P=[++sp]
                                    seq_stage <= stg_PLP;

                                when x"AA" =>                       -- TAX X=A
                                    seq_stage <= stg_TAX;
                                when x"8A" =>                       -- TXA A=X
                                    seq_stage <= stg_TXA;
                                when x"A8" =>                       -- TAY Y=A
                                    seq_stage <= stg_TAY;
                                when x"98" =>                       -- TYA A=Y
                                    seq_stage <= stg_TYA;

                                when x"CA" =>                       -- DEX X=X-1
                                    seq_stage <= stg_DEX;
                                when x"88" =>                       -- DEY Y=Y-1
                                    seq_stage <= stg_DEY;
                                when x"C6" =>                       -- DEC --[mem]
                                    ret_stage <= stg_decmem;        --  zp
                                    seq_stage <= stg_sub_zp;
                                when x"D6" =>
                                    ret_stage <= stg_decmem;        --  zp+x
                                    seq_stage <= stg_sub_zpx;
                                when x"CE" =>
                                    ret_stage <= stg_decmem;        --  abs
                                    seq_stage <= stg_sub_abs;
                                when x"DE" =>
                                    ret_stage <= stg_decmem;        --  abs+x
                                    seq_stage <= stg_sub_absx;

                                when x"E8" =>                       -- INX X=X+1
                                    seq_stage <= stg_INX;
                                when x"C8" =>                       -- INY Y=Y+1
                                    seq_stage <= stg_INY;
                                when x"E6" =>                       -- INC ++[mem]
                                    ret_stage <= stg_incmem;        --  zp
                                    seq_stage <= stg_sub_zp;
                                when x"F6" =>
                                    ret_stage <= stg_incmem;        --  zp+x
                                    seq_stage <= stg_sub_zpx;
                                when x"EE" =>
                                    ret_stage <= stg_incmem;        --  abs
                                    seq_stage <= stg_sub_abs;
                                when x"FE" =>
                                    ret_stage <= stg_incmem;        --  abs+x
                                    seq_stage <= stg_sub_absx;

                                when x"10" =>                       -- BPL PC+OP when N=0
                                    seq_stage <= stg_BPL;
                                when x"30" =>                       -- BMI PC+OP when N=1
                                    seq_stage <= stg_BMI;
                                when x"50" =>                       -- BVC PC+OP when V=0
                                    seq_stage <= stg_BVC;
                                when x"70" =>                       -- BVS PC+OP when V=1
                                    seq_stage <= stg_BVS;
                                when x"90" =>                       -- BCC PC+OP when C=0
                                    seq_stage <= stg_BCC;
                                when x"B0" =>                       -- BCS PC+OP when C=1
                                    seq_stage <= stg_BCS;
                                when x"D0" =>                       -- BNE PC+OP when Z=0
                                    seq_stage <= stg_BNE;
                                when x"F0" =>                       -- BEQ PC+OP when Z=1
                                    seq_stage <= stg_BEQ;

                                when x"0A" =>                       -- ASL  C  << T << '0'
                                    seq_stage <= stg_ASL_a;         --  a
                                when x"06" =>
                                    ret_stage <= stg_ASLmem;         --  zp
                                    seq_stage <= stg_sub_zp;
                                when x"16" =>
                                    ret_stage <= stg_ASLmem;         --  zp+x
                                    seq_stage <= stg_sub_zpx;
                                when x"0E" =>
                                    ret_stage <= stg_ASLmem;         --  abs
                                    seq_stage <= stg_sub_abs;
                                when x"1E" =>
                                    ret_stage <= stg_ASLmem;         --  abs+x
                                    seq_stage <= stg_sub_absx;

                                when x"2A" =>                       -- ROL  C  << T <<  C
                                    seq_stage <= stg_ROL_a;         --  a
                                when x"26" =>
                                    ret_stage <= stg_ROLmem;        --  zp
                                    seq_stage <= stg_sub_zp;
                                when x"36" =>
                                    ret_stage <= stg_ROLmem;        --  zp+x
                                    seq_stage <= stg_sub_zpx;
                                when x"2E" =>
                                    ret_stage <= stg_ROLmem;        --  abs
                                    seq_stage <= stg_sub_abs;
                                when x"3E" =>
                                    ret_stage <= stg_ROLmem;        --  abs+x
                                    seq_stage <= stg_sub_absx;

                                when x"4A" =>                       -- LSR '0' >> T >>  C
                                    seq_stage <= stg_LSR_a;         --  a
                                when x"46" =>
                                    ret_stage <= stg_LSRmem;        --  zp
                                    seq_stage <= stg_sub_zp;
                                when x"56" =>
                                    ret_stage <= stg_LSRmem;        --  zp+x
                                    seq_stage <= stg_sub_zpx;
                                when x"4E" =>
                                    ret_stage <= stg_LSRmem;        --  abs
                                    seq_stage <= stg_sub_abs;
                                when x"5E" =>
                                    ret_stage <= stg_LSRmem;        --  abs+x
                                    seq_stage <= stg_sub_absx;

                                when x"6A" =>                       -- ROR  C  >> T >>  C
                                    seq_stage <= stg_ROR_a;         --  a
                                when x"66" =>
                                    ret_stage <= stg_ROLmem;        --  zp
                                    seq_stage <= stg_sub_zp;
                                when x"76" =>
                                    ret_stage <= stg_ROLmem;        --  zp+x
                                    seq_stage <= stg_sub_zpx;
                                when x"6E" =>
                                    ret_stage <= stg_ROLmem;        --  abs
                                    seq_stage <= stg_sub_abs;
                                when x"7E" =>
                                    ret_stage <= stg_ROLmem;        --  abs+x
                                    seq_stage <= stg_sub_absx;

                                when x"60" =>                       -- RTS PCH=[++SP],PCL=[++SP],++PC
                                    seq_stage <= stg_RTS;
                                when x"40" =>                       -- RTI P=[++SP], PCH=[++SP],PCL=[++SP]
                                    seq_stage <= stg_RTI;
                                when x"20" =>                       -- JSR PC@OP.h to stack, PC=OP.w
                                    seq_stage <= stg_JSR;

                                when others =>
                                    seq_stage <= stg_tail;          -- NOP
                            end case;
                        else
                            abus_off <= '1';                        -- burn a full PH1/PH2 cycle if RDY=0
                            dbctl_off <= '1';
                            seq <= x"08";
                        end if;
                    when x"08" => seq <= countSeq(seq);         -- PH1+: burn
                    when x"09" => seq <= countSeq(seq);         -- PH1-: burn
                    when x"0A" => seq <= x"07";                 -- PH2+: will check RDY again on PH2-
                    when others => null;
                end case;
            end if;
            
            -- epilogue stage (also handles NOP)
            -- checks for interrupts
            if (seq_stage = stg_tail) then
                case seq is
                    when x"00" =>                               -- PH1+: burn
                        abus_off <= '0';                        --       abus enabled
                        dbctl_off <= '0';                       --       dbus enabled
                        dbctl_r1w0 <= '1';                      --       dbus to read
                        seq <= countSeq(seq);
                    when x"01" =>                               -- PH1-: burn
                        seq <= countSeq(seq);
                    when x"02" =>                               -- PH2+: burn
                            seq <= countSeq(seq);
                    when x"03" =>
                        seq_stage <= stg_fetch;                 -- PH2-: return to fetch (on PH1+)
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_sub_incpc) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        ALUain <= reg_pcl;
                        alu_cin_mode <= cin_set;
                        alu_bin_mode <= bin_clr;
                        alu_din_mode <= din_clr;
                        aop <= aop_add;
                        seq <= countSeq(seq);
                    when x"01" =>                               -- PH1-: store PCL=PCL+1
                        reg_pcl <= ALUrout;
                        private_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"02" =>                               -- PH2+:
                        ALUain <= reg_pch;
                        alu_cin_mode <= cin_aux;                --       apply carry from PCL+1
                        seq <= countSeq(seq);
                    when x"03" =>                               -- PH2-:
                        reg_pch <= ALUrout;                     --       store PCH=PCH+C
                        seq <= x"00";
                        seq_stage <= ret_stage;                 -- PH2+: set return-to stage (on PH1+)
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_incmem) then                    --        ++[MEM]
                case seq is
                    when x"00" =>                               -- PH1+:
                        dbctl_off <= '0';                       --      BUS: read from [MEM]
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;
                        alu_cin_mode <= cin_set;                --       ALU: Rout=Ain+1
                        alu_bin_mode <= bin_clr;
                        alu_din_mode <= din_clr;
                        aop <= aop_add;
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data)
                        ALUain <= regbus;                       --       [MEM] => Ain
                        seq <= countSeq(seq);
                    when x"04" =>                               -- PH1+:
                        dbctl_r1w0 <= '0';                      --       BUS: write to [MEM]
                        psw_z  <= ALUzout;                      --       NZ,Dout=[MEM]+1
                        psw_n  <= ALUnout;
                        outval <= ALUrout;
                        seq <= countSeq(seq);
                    when x"05" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"06" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"07" =>                               -- PH2-: (valid data)
                        seq <= x"00";
                        seq_stage <= stg_tail;                  --       instruction finished
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_decmem) then                    --        --[MEM]
                case seq is
                    when x"00" =>                               -- PH1+:
                        dbctl_off <= '0';                       --      BUS: read from [MEM]
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;
                        alu_cin_mode <= cin_clr;                --       ALU: Rout=Ain-1
                        alu_bin_mode <= bin_set;
                        alu_din_mode <= din_clr;
                        aop <= aop_add;
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data)
                        ALUain <= regbus;                       --       [MEM] => Ain
                        seq <= countSeq(seq);
                    when x"04" =>                               -- PH1+:
                        dbctl_r1w0 <= '0';                      --       BUS: write to [MEM]
                        psw_z  <= ALUzout;                      --       NZ,Dout=[MEM]+1
                        psw_n  <= ALUnout;
                        outval <= ALUrout;
                        seq <= countSeq(seq);
                    when x"05" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"06" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"07" =>                               -- PH2-: (valid data)
                        seq <= x"00";
                        seq_stage <= stg_tail;                  --       instruction finished
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_reljmp) then
                case seq is                                     -- PC+=(signed)BUF
                    when x"00" =>                               -- PH1+:
                        ALUain <= reg_pcl;
                        ALUbin <= buf_data;
                        alu_cin_mode <= cin_clr;
                        alu_bin_mode <= bin_reg;
                        alu_din_mode <= din_clr;
                        aop <= aop_add;
                        seq <= countSeq(seq);
                    when x"01" =>                               -- PH1-: store PCL=PCL+BUF
                        reg_pcl <= ALUrout;
                        private_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"02" =>                               -- PH2+:
                        ALUain <= reg_pch;
                        ALUbin <= sgn(buf_data);                --       sign extend BUF for PCH
                        alu_cin_mode <= cin_aux;                --       apply carry from PCL+BUF
                        seq <= countSeq(seq);
                    when x"03" =>                               -- PH2-:
                        reg_pch <= ALUrout;                     --       store PCH=PCH+C+sgn(BUF)
                        seq <= x"00";
                        seq_stage <= stg_tail;                  --       branch finished
                    when others => null;
                end case;
            end if;

            -- JMP abs
            if (seq_stage = stg_JMP_abs) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= reg_pc;
                        ALUain <= reg_pcl;
                        alu_bin_mode <= bin_clr;
                        alu_cin_mode <= cin_set;
                        seq <= countSeq(seq);
                    when x"01" =>                           -- PH1-: (valid addr)
                        reg_pcl <= ALUrout;
                        private_c <= ALUcout;               --       ++PC
                        seq <= countSeq(seq);
                    when x"02" =>                           -- PH2+: pass
                        ALUain <= reg_pch;
                        alu_bin_mode <= bin_clr;
                        alu_cin_mode <= cin_aux;
                        seq <= countSeq(seq);
                    when x"03" =>                           -- PH2-: (valid data) write to MEML
                        buf_addr_l <= regbus;               --       (we can't store to PC as we're using it)
                        reg_pch <= ALUrout;
                        seq <= countSeq(seq);
                    when x"04" =>                           -- PH1+: put PC (+1) on abus
                        abus <= reg_pc;
                        seq <= countSeq(seq);
                    when x"05" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"06" => seq <= countSeq(seq);     -- PH2+: pass
                        seq <= countSeq(seq);
                    when x"07" =>                           -- PH2-: (valid data) write to PCH
                        reg_pch <= regbus;
                        reg_pcl <= buf_addr_l;              --       copy buffered lobyte to PCL
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- JMP ind
            if (seq_stage = stg_JMP_ind) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= reg_pc;
                        ALUain <= reg_pcl;
                        alu_bin_mode <= bin_clr;
                        alu_cin_mode <= cin_set;
                        seq <= countSeq(seq);
                    when x"01" =>                           -- PH1-: (valid addr)
                        reg_pcl <= ALUrout;
                        private_c <= ALUcout;               --       ++PC
                        seq <= countSeq(seq);
                    when x"02" =>                           -- PH2+: pass
                        ALUain <= reg_pch;
                        alu_cin_mode <= cin_aux;
                        seq <= countSeq(seq);
                    when x"03" =>                           -- PH2-: (valid data) write to MEML
                        buf_addr_l <= regbus;               --       save to MEML
                        reg_pch <= ALUrout;
                        seq <= countSeq(seq);
                    when x"04" =>                           -- PH1+: put PC (+1) on abus
                        abus <= reg_pc;
                        seq <= countSeq(seq);
                    when x"05" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"06" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"07" =>                           -- PH2-: (valid data) write to PCH
                        buf_addr_h <= regbus;               --       save to MEMH
                        seq <= countSeq(seq);
                    when x"08" =>                           -- PH1+:
                        abus <= buf_addr;                   --       put MEM on abus
                        ALUain <= buf_addr_l;               --       set up for ++MEM
                        ALU_cin_mode <= cin_set;
                        seq <= countSeq(seq);
                    when x"09" =>                           -- PH1-: (valid addr)
                        buf2l <= ALUrout;
                        private_c <= ALUcout;               --       BUF2=MEM+1
                        seq <= countSeq(seq);
                    when x"0a" =>                           -- PH2+:
                        ALUain <= buf_addr_h;               --
                        ALU_cin_mode <= cin_aux;
                        seq <= countSeq(seq);
                    when x"0b" =>                           -- PH2-: (valid data)
                        reg_pcl <= regbus;                  --       PCL=(MEM)
                        buf_addr_l <= buf2l;
                        buf_addr_h <= ALUrout;              --       ++MEM
                        seq <= countSeq(seq);
                    when x"0c" =>                           -- PH1+:
                        abus <= buf_addr;                   --       put MEM (+1) on abus
                        seq <= countSeq(seq);
                    when x"0d" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"0e" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"0f" =>                           -- PH2-: (valid data)
                        reg_pch <= regbus;                  --       PCH=MEM (+1)
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_mem2buf) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC (+1) on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;                   --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"03" =>                           -- PH2-: (valid data)
                        buf_data <= regbus;                  --      save to BUF
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;
 
            if (seq_stage = stg_mem2a) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC (+1) on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;                   --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"03" =>                           -- PH2-: (valid data)
                        reg_a <= regbus;                    --       save to A
                        psw_z <= isZero(regbus);            --       Z flag
                        psw_n <= regbus(7);                 --       N flag
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_aANDmem) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC (+1) on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;                   --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"03" =>                           -- PH2-: (valid data)
                        reg_a <= reg_A and regbus;          --       A = A & [mem]
                        psw_z <= isZero(reg_A and regbus);  --       Z flag
                        psw_n <= getb(reg_A and regbus,7);  --       N flag
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_BITmem) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC (+1) on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;                   --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"03" =>                           -- PH2-: (valid data)
                        psw_z <= isZero(regbus and reg_A);  --       Z flag as if A & [mem]
                        psw_n <= regbus(7);                 --       N flag <= [mem][7]
                        psw_v <= regbus(6);                 --       V flag <= [mem][6]
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_aADDmem) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC (+1) on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;                   --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"03" =>                           -- PH2-: (valid data)
                        alu_din_mode <= din_psw;            --       P.D => din
                        alu_cin_mode <= cin_psw;            --       P.C => cin
                        alu_bin_mode <= bin_reg;            --       reg => bin
                        ALUain <= reg_a;                    --       A
                        aop <= aop_add;                     --         + P.C +                     
                        ALUbin <= regbus;                   --                 mem
                        seq <= countSeq(seq);
                    when x"04" =>                           -- PH1+:
                        reg_a <= ALUrout;                   --       store result
                        psw_n <= ALUnout;
                        psw_v <= ALUvout;
                        psw_z <= ALUzout;
                        psw_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"05" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"06" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"07" =>                           -- PH2-: (valid data)
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_aSUBmem) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC (+1) on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;                   --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"03" =>                           -- PH2-: (valid data)
                        alu_din_mode <= din_psw;            --        P.D => din
                        alu_cin_mode <= cin_psw;            --        P.C => cin
                        alu_bin_mode <= bin_ireg;           --       ^reg => bin (1's compliment)
                        ALUain <= reg_a;                    --       A
                        aop <= aop_add;                     --         + P.C -                     
                        ALUbin <= regbus;                   --                 (mem+1)
                        seq <= countSeq(seq);
                    when x"04" =>                           -- PH1+:
                        reg_a <= ALUrout;                   --       store result
                        psw_n <= ALUnout;
                        psw_v <= ALUvout;
                        psw_z <= ALUzout;
                        psw_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"05" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"06" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"07" =>                           -- PH2-: (valid data)
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_aCMPmem) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC (+1) on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;                   --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"03" =>                           -- PH2-: (valid data)
                        alu_din_mode <= din_clr;            --          0 => din
                        alu_cin_mode <= cin_set;            --          1 => cin
                        alu_bin_mode <= bin_ireg;           --       ^reg => bin (1's compliment)
                        ALUain <= reg_a;                    --       A
                        aop <= aop_add;                     --         + 1 -                     
                        ALUbin <= regbus;                   --               (mem+1)
                        seq <= countSeq(seq);
                    when x"04" =>                           -- PH1+:
                        psw_n <= ALUnout;                   --       store result (NZC only)
                        psw_z <= ALUzout;
                        psw_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"05" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"06" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"07" =>                           -- PH2-: (valid data)
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_xCMPmem) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC (+1) on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;                   --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"03" =>                           -- PH2-: (valid data)
                        alu_din_mode <= din_clr;            --          0 => din
                        alu_cin_mode <= cin_set;            --          1 => cin
                        alu_bin_mode <= bin_ireg;           --       ^reg => bin (1's compliment)
                        ALUain <= reg_x;                    --       X
                        aop <= aop_add;                     --         + 1 -                     
                        ALUbin <= regbus;                   --               (mem+1)
                        seq <= countSeq(seq);
                    when x"04" =>                           -- PH1+:
                        psw_n <= ALUnout;                   --       store result (NZC only)
                        psw_z <= ALUzout;
                        psw_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"05" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"06" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"07" =>                           -- PH2-: (valid data)
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_yCMPmem) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC (+1) on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;                   --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"03" =>                           -- PH2-: (valid data)
                        alu_din_mode <= din_clr;            --          0 => din
                        alu_cin_mode <= cin_set;            --          1 => cin
                        alu_bin_mode <= bin_ireg;           --       ^reg => bin (1's compliment)
                        ALUain <= reg_y;                    --       Y
                        aop <= aop_add;                     --         + 1 -                     
                        ALUbin <= regbus;                   --               (mem+1)
                        seq <= countSeq(seq);
                    when x"04" =>                           -- PH1+:
                        psw_n <= ALUnout;                   --       store result (NZC only)
                        psw_z <= ALUzout;
                        psw_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"05" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"06" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"07" =>                           -- PH2-: (valid data)
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_aORmem) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC (+1) on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;                   --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"03" =>                           -- PH2-: (valid data)
                        reg_a <= reg_A or regbus;           --       A = A | [mem]
                        psw_z <= isZero(reg_A or regbus);   --       Z flag
                        psw_n <= getb(reg_A or regbus,7);   --       N flag
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_aXORmem) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC (+1) on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;                   --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"03" =>                           -- PH2-: (valid data)
                        reg_a <= reg_A xor regbus;          --       A = A ^ [mem]
                        psw_z <= isZero(reg_A xor regbus);  --       Z flag
                        psw_n <= getb(reg_A xor regbus,7);  --       N flag
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_mem2x) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC (+1) on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;                   --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"03" =>                           -- PH2-: (valid data)
                        reg_x <= regbus;                    --       save to X
                        psw_z <= isZero(regbus);            --       Z flag
                        psw_n <= regbus(7);                 --       N flag
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_mem2y) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC (+1) on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;                   --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"03" =>                           -- PH2-: (valid data)
                        reg_y <= regbus;                    --       save to Y
                        psw_z <= isZero(regbus);            --       Z flag
                        psw_n <= regbus(7);                 --       N flag
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_a2mem) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC (+1) on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '0';                  --       write
                        abus_off <= '0';
                        abus <= buf_addr;                   --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"02" =>                           -- PH2+:
                        outval <= reg_a;                    --       A to data out
                        seq <= countSeq(seq);
                    when x"03" =>                           -- PH2-: (valid data)
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_x2mem) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC (+1) on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '0';                  --       write
                        abus_off <= '0';
                        abus <= buf_addr;                   --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"02" =>                           -- PH2+:
                        outval <= reg_x;                    --       X to data out
                        seq <= countSeq(seq);
                    when x"03" =>                           -- PH2-: (valid data)
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_y2mem) then
                case seq is
                    when x"00" =>                           -- PH1+: put PC (+1) on abus
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '0';                  --       write
                        abus_off <= '0';
                        abus <= buf_addr;                   --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"02" =>                           -- PH2+:
                        outval <= reg_y;                    --       Y to data out
                        seq <= countSeq(seq);
                    when x"03" =>                           -- PH2-: (valid data)
                        seq_stage <= stg_tail;              --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_sub_imm) then
                case seq is
                    when x"00" =>                               -- PH1+: sync on, PC to abus
                        abus_off <= '0';
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus on for read
                        buf_addr <= reg_pc;                     --       MEM=PC
                        ALUain <= reg_pcl;
                        alu_cin_mode <= cin_set;
                        alu_bin_mode <= bin_clr;
                        seq <= countSeq(seq);
                    when x"01" =>                               -- PH1-: (valid addr)
                        reg_pcl <= ALUrout;
                        private_c <= ALUcout;
                        alu_cin_mode <= cin_aux;
                        ALUain <= reg_pch;
                        seq <= countSeq(seq);
                    when x"02" =>                               -- PH2+:
                        reg_pch <= ALUrout;                     --       PC=PC+1
                        seq <= countSeq(seq);
                    when x"03" =>                               -- PH2-: (valid data) store data to A
                        seq <= x"00";
                        seq_stage <= ret_stage;
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_sub_abs) then
                case seq is
                    when x"00" =>                           -- PH1+: sync on, PC to abus
                        abus_off <= '0';
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                  --       dbus on for read
                        ALUain <= reg_pcl;                  --       set up for ++PC
                        alu_bin_mode <= bin_clr;
                        alu_cin_mode <= cin_set;
                        seq <= countSeq(seq);
                    when x"01" =>                           -- PH1-: (valid addr)
                        buf2l <= ALUrout;                   --       save to temp since PC in use
                        private_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"02" =>                           -- PH2+:
                        ALUain <= reg_pch;
                        alu_cin_mode <= cin_aux;
                        seq <= countSeq(seq);
                    when x"03" =>                           -- PH2-: (valid data) get MEML
                        reg_pcl <= buf2l;
                        reg_pch <= ALUrout;                 --       ++PC
                        buf_addr_l <= regbus;
                        seq <= countSeq(seq);
                    when x"04" =>                           -- PH1+: put PC (+1) on abus
                        abus <= reg_pc;
                        alu_cin_mode <= cin_set;
                        ALUain <= reg_pcl;
                        seq <= countSeq(seq);
                    when x"05" =>                           -- PH1-: (valid addr) pass
                        buf2l <= ALUrout;                   --       save to temp since PC in use
                        private_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"06" =>                           -- PH2+:
                        ALUain <= reg_pch;
                        alu_cin_mode <= cin_aux;
                        seq <= countSeq(seq);
                    when x"07" =>                           -- PH2-: (valid data) write to PCH
                        reg_pcl <= buf2l;
                        reg_pch <= ALUrout;                 --       ++PC
                        buf_addr_h <= regbus;               --       save to MEMH
                        seq_stage <= ret_stage;
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_sub_absx) then
                case seq is
                    when x"00" =>                           -- PH1+: sync on, PC to abus
                        abus_off <= '0';
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                  --       dbus on for read
                        ALUain <= reg_pcl;                  --       set up for ++PC
                        alu_bin_mode <= bin_clr;
                        alu_cin_mode <= cin_set;
                        seq <= countSeq(seq);
                    when x"01" =>                           -- PH1-: (valid addr)
                        buf2l <= ALUrout;                   --       save to temp since PC in use
                        private_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"02" =>                           -- PH2+:
                        ALUain <= reg_pch;
                        alu_cin_mode <= cin_aux;
                        seq <= countSeq(seq);
                    when x"03" =>                           -- PH2-: (valid data) get MEML
                        reg_pcl <= buf2l;
                        reg_pch <= ALUrout;                 --       ++PC
                        buf_addr_l <= regbus;
                        seq <= countSeq(seq);
                    when x"04" =>                           -- PH1+: put PC (+1) on abus
                        abus <= reg_pc;
                        alu_cin_mode <= cin_set;
                        ALUain <= reg_pcl;
                        seq <= countSeq(seq);
                    when x"05" =>                           -- PH1-: (valid addr) pass
                        buf2l <= ALUrout;                   --       save to temp since PC in use
                        private_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"06" =>                           -- PH2+:
                        ALUain <= reg_pch;
                        alu_cin_mode <= cin_aux;
                        seq <= countSeq(seq);
                    when x"07" =>                           -- PH2-: (valid data) write to PCH
                        reg_pcl <= buf2l;
                        reg_pch <= ALUrout;                 --       ++PC
                        buf_addr_h <= regbus;               --       save to MEMH
                        seq <= countSeq(seq);
                    when x"08" =>                           -- PH1+:
                        alu_bin_mode <= bin_reg;            --       now do MEM+X
                        alu_cin_mode <= cin_clr;
                        ALUain <= buf_addr_l;
                        ALUbin <= reg_x;                    --       MEML+X
                        seq <= countSeq(seq);
                    when x"09" =>                           -- PH1-: (valid addr) pass
                        buf_addr_l <= ALUrout;              --       MEML=MEML+X
                        private_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"0A" =>                           -- PH2+:
                        ALUain <= buf_addr_h;
                        alu_bin_mode <= bin_clr;
                        alu_cin_mode <= cin_aux;            --       MEMH+C
                        seq <= countSeq(seq);
                    when x"0B" =>                           -- PH2-: (valid data)
                        buf_addr_h <= ALUrout;              --       MEMH=MEMH+C
                        seq_stage <= ret_stage;
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_sub_absy) then
                case seq is
                    when x"00" =>                           -- PH1+: sync on, PC to abus
                        abus_off <= '0';
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                  --       dbus on for read
                        ALUain <= reg_pcl;                  --       set up for ++PC
                        alu_bin_mode <= bin_clr;
                        alu_cin_mode <= cin_set;
                        seq <= countSeq(seq);
                    when x"01" =>                           -- PH1-: (valid addr)
                        buf2l <= ALUrout;                   --       save to temp since PC in use
                        private_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"02" =>                           -- PH2+:
                        ALUain <= reg_pch;
                        alu_cin_mode <= cin_aux;
                        seq <= countSeq(seq);
                    when x"03" =>                           -- PH2-: (valid data) get MEML
                        reg_pcl <= buf2l;
                        reg_pch <= ALUrout;                 --       ++PC
                        buf_addr_l <= regbus;
                        seq <= countSeq(seq);
                    when x"04" =>                           -- PH1+: put PC (+1) on abus
                        abus <= reg_pc;
                        alu_cin_mode <= cin_set;
                        ALUain <= reg_pcl;
                        seq <= countSeq(seq);
                    when x"05" =>                           -- PH1-: (valid addr) pass
                        buf2l <= ALUrout;                   --       save to temp since PC in use
                        private_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"06" =>                           -- PH2+:
                        ALUain <= reg_pch;
                        alu_cin_mode <= cin_aux;
                        seq <= countSeq(seq);
                    when x"07" =>                           -- PH2-: (valid data) write to PCH
                        reg_pcl <= buf2l;
                        reg_pch <= ALUrout;                 --       ++PC
                        buf_addr_h <= regbus;               --       save to MEMH
                        seq <= countSeq(seq);
                    when x"08" =>                           -- PH1+:
                        alu_bin_mode <= bin_reg;            --       now do MEM+Y
                        alu_cin_mode <= cin_clr;
                        ALUain <= buf_addr_l;
                        ALUbin <= reg_y;                    --       MEML+Y
                        seq <= countSeq(seq);
                    when x"09" =>                           -- PH1-: (valid addr)
                        buf_addr_l <= ALUrout;              --       MEML=MEML+Y
                        private_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"0A" =>                           -- PH2+:
                        ALUain <= buf_addr_h;
                        alu_bin_mode <= bin_clr;
                        alu_cin_mode <= cin_aux;            --       MEMH+C
                        seq <= countSeq(seq);
                    when x"0B" =>                           -- PH2-: (valid data)
                        buf_addr_h <= ALUrout;              --       MEMH=MEMH+C
                        seq_stage <= ret_stage;
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_sub_zp) then
                case seq is
                    when x"00" =>                           -- PH1+: sync on, PC to abus
                        abus_off <= '0';
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                  --       dbus on for read
                        ALUain <= reg_pcl;                  --       set up for ++PC
                        alu_bin_mode <= bin_clr;
                        alu_cin_mode <= cin_set;
                        seq <= countSeq(seq);
                    when x"01" =>                           -- PH1-: (valid addr)
                        buf2l <= ALUrout;                   --       save to temp since PC in use
                        private_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"02" =>                           -- PH2+:
                        ALUain <= reg_pch;
                        alu_cin_mode <= cin_aux;
                        seq <= countSeq(seq);
                    when x"03" =>                           -- PH2-: (valid data) get MEML
                        buf_addr_h <= x"00";
                        buf_addr_l <= regbus;               --       MEM = 00:[PC]
                        reg_pcl <= buf2l;
                        reg_pch <= ALUrout;                 --       ++PC
                        seq_stage <= ret_stage;
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_sub_zpx) then
                case seq is
                    when x"00" =>                           -- PH1+: sync on, PC to abus
                        abus_off <= '0';
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                  --       dbus on for read
                        ALUain <= reg_pcl;                  --       set up for ++PC
                        alu_bin_mode <= bin_clr;
                        alu_cin_mode <= cin_set;
                        seq <= countSeq(seq);
                    when x"01" =>                           -- PH1-: (valid addr)
                        buf2l <= ALUrout;                   --       save to temp since PC in use
                        private_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"02" =>                           -- PH2+:
                        ALUain <= reg_pch;
                        alu_cin_mode <= cin_aux;
                        seq <= countSeq(seq);
                    when x"03" =>                           -- PH2-: (valid data) get MEML
                        buf_addr_h <= x"00";
                        buf_addr_l <= regbus;               --       MEM = 00:[PC]
                        reg_pcl <= buf2l;
                        reg_pch <= ALUrout;                 --       ++PC
                        seq <= countSeq(seq);
                    when x"04" =>                           -- PH1+:
                        alu_bin_mode <= bin_reg;            --       now do MEML+X
                        alu_cin_mode <= cin_clr;
                        ALUain <= buf_addr_l;
                        ALUbin <= reg_x;                    --       MEML+X
                        seq <= countSeq(seq);
                    when x"05" =>                           -- PH1-: (valid addr)
                        buf_addr_l <= ALUrout;              --       MEML=MEML+X
                        seq <= countSeq(seq);
                    when x"06" => seq <= countSeq(seq);     -- PH2+: pass (ZP offset wraps around)
                    when x"07" =>                           -- PH2-: (valid data)
                        seq_stage <= ret_stage;
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_sub_indx) then
                case seq is
                    when x"00" =>                           -- PH1+: sync on, PC to abus
                        abus_off <= '0';
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                  --       dbus on for read
                        ALUain <= reg_pcl;                  --       set up for ++PC
                        alu_bin_mode <= bin_clr;
                        alu_cin_mode <= cin_set;
                        seq <= countSeq(seq);
                    when x"01" =>                           -- PH1-: (valid addr)
                        buf2l <= ALUrout;                   --       save to temp since PC in use
                        private_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"02" =>                           -- PH2+:
                        ALUain <= reg_pch;
                        alu_cin_mode <= cin_aux;
                        seq <= countSeq(seq);
                    when x"03" =>                           -- PH2-: (valid data) get MEML
                        buf_addr_l <= regbus;               --       MEM = 00:[PC]
                        reg_pcl <= buf2l;
                        reg_pch <= ALUrout;                 --       ++PC
                        seq <= countSeq(seq);
                    when x"04" =>                           -- PH1+:
                        alu_bin_mode <= bin_reg;            --       now do MEML+X
                        alu_cin_mode <= cin_clr;
                        ALUain <= buf_addr_l;
                        ALUbin <= reg_x;                    --       MEML+X
                        seq <= countSeq(seq);
                    when x"05" =>                           -- PH1-: (valid addr)
                        buf2l <= ALUrout;                   --       BUF2=00:MEML+X
                        buf2h <= x"00";
                        seq <= countSeq(seq);
                    when x"06" => seq <= countSeq(seq);     -- PH2+: pass (ZP offset wraps around)
                    when x"07" => seq <= countSeq(seq);     -- PH2-: (valid data) pass
                    when x"08" =>                           -- PH1+:
                        abus <= buf2;                       --       buf2 on abus
                        seq <= countSeq(seq);
                    when x"09" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"0a" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"0b" =>                           -- PH2-: (valid data)
                        buf_addr_l <= regbus;               --       MEML=[buf2++]
                        buf2 <= inc16(buf2);
                        seq <= countSeq(seq);
                    when x"0c" =>                           -- PH1+:
                        abus <= buf2;                       --       buf2 (+1) on abus
                        seq <= countSeq(seq);
                    when x"0d" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"0e" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"0f" =>                           -- PH2-: (valid data)
                        buf_addr_h <= regbus;               --       MEMH=[buf2] (+1)
                        seq_stage <= ret_stage;
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_sub_zpy) then
                case seq is
                    when x"00" =>                           -- PH1+: sync on, PC to abus
                        abus_off <= '0';
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                  --       dbus on for read
                        ALUain <= reg_pcl;                  --       set up for ++PC
                        alu_bin_mode <= bin_clr;
                        alu_cin_mode <= cin_set;
                        seq <= countSeq(seq);
                    when x"01" =>                           -- PH1-: (valid addr)
                        buf2l <= ALUrout;                   --       save to temp since PC in use
                        private_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"02" =>                           -- PH2+:
                        ALUain <= reg_pch;
                        alu_cin_mode <= cin_aux;
                        seq <= countSeq(seq);
                    when x"03" =>                           -- PH2-: (valid data) get MEML
                        buf_addr_h <= x"00";
                        buf_addr_l <= regbus;               --       MEM = 00:[PC]
                        reg_pcl <= buf2l;
                        reg_pch <= ALUrout;                 --       ++PC
                        seq <= countSeq(seq);
                    when x"04" =>                           -- PH1+:
                        alu_bin_mode <= bin_reg;            --       now do MEML+Y
                        alu_cin_mode <= cin_clr;
                        ALUain <= buf_addr_l;
                        ALUbin <= reg_y;                    --       MEML+Y
                        seq <= countSeq(seq);
                    when x"05" =>                           -- PH1-: (valid addr)
                        buf_addr_l <= ALUrout;              --       MEML=MEML+Y
                        seq <= countSeq(seq);
                    when x"06" => seq <= countSeq(seq);     -- PH2+: pass (ZP offset wraps around)
                    when x"07" =>                           -- PH2-: (valid data)
                        seq_stage <= ret_stage;
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_sub_indy) then
                case seq is
                    when x"00" =>                           -- PH1+: sync on, PC to abus
                        abus_off <= '0';
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                  --       dbus on for read
                        ALUain <= reg_pcl;                  --       set up for ++PC
                        alu_bin_mode <= bin_clr;
                        alu_cin_mode <= cin_set;
                        alu_din_mode <= din_clr;
                        seq <= countSeq(seq);
                    when x"01" =>                           -- PH1-: (valid addr)
                        buf2l <= ALUrout;                   --       save to temp since PC in use
                        buf2h <= x"00";
                        private_c <= ALUcout;
                        seq <= countSeq(seq);
                    when x"02" =>                           -- PH2+:
                        ALUain <= reg_pch;
                        alu_cin_mode <= cin_aux;
                        seq <= countSeq(seq);
                    when x"03" =>                           -- PH2-: (valid data)
                        buf_addr_l <= regbus;               --       MEML = [PC]
                        reg_pcl <= buf2l;
                        reg_pch <= ALUrout;                 --       ++PC
                        seq <= countSeq(seq);
                    when x"04" =>                           -- PH1+:
                        abus <= x"00" & buf_addr_l;         --       put 00:MEML on abus
                        buf2 <= x"00" & buf_addr_l;         --       buf2=00:MEML
                        seq <= countSeq(seq);
                    when x"05" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"06" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"07" =>                           -- PH2-: (valid data) pass
                        buf_addr_l <= regbus;               --      MEML = [BUF2]
                        buf2 <= inc16(buf2);                --      ++buf2
                        seq <= countSeq(seq);
                    when x"08" =>                           -- PH1+:
                        abus <= buf2;                       --       buf2 (buf+1) on abus
                        ALUain <= buf_addr_l;               --       start MEM=MEM+Y
                        ALUbin <= reg_y;                    --       MEML+Y
                        ALU_cin_mode <= cin_clr;
                        ALU_bin_mode <= bin_reg;
                        seq <= countSeq(seq);
                    when x"09" =>                           -- PH1-: (valid addr)
                        buf_addr_l <= ALUrout;              --       MEML=MEML+Y
                        private_c <= ALUcout;
                        alu_cin_mode <= cin_aux;
                        alu_bin_mode <= bin_clr;
                        seq <= countSeq(seq);
                    when x"0a" =>                           -- PH2+:
                        seq <= countSeq(seq);
                    when x"0b" =>                           -- PH2-: (valid data)
                        ALUain <= regbus;                   --       C+[BUF2] (MSB)
                        seq <= countSeq(seq);
                    when x"0c" =>                           -- PH1+:
                        buf_addr_h <= ALUrout;              --      MEMH=C+[BUF2] (MSB)
                        seq <= countSeq(seq);               --      MEM=MEM+Y done
                    when x"0d" => seq <= countSeq(seq);     -- PH1-: (valid addr) pass
                    when x"0e" => seq <= countSeq(seq);     -- PH2+: pass
                    when x"0f" =>                           -- PH2-: (valid data)
                        seq_stage <= ret_stage;
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- CLC
            if (seq_stage = stg_CLC) then
                case seq is
                    when x"00" =>                               -- PH1+: C=0
                        psw_c <= '0';
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- SEC
            if (seq_stage = stg_SEC) then
                case seq is
                    when x"00" =>                               -- PH1+: C=1
                        psw_c <= '1';
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- CLI
            if (seq_stage = stg_CLI) then
                case seq is
                    when x"00" =>                               -- PH1+: I=0
                        psw_i <= '0';
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- SEI
            if (seq_stage = stg_SEI) then
                case seq is
                    when x"00" =>                               -- PH1+: I=1
                        psw_i <= '1';
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- CLV
            if (seq_stage = stg_CLV) then
                case seq is
                    when x"00" =>                               -- PH1+: V=0
                        psw_v <= '0';
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- CLD
            if (seq_stage = stg_CLD) then
                case seq is
                    when x"00" =>                               -- PH1+: D=0
                        psw_d <= '0';
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- SED
            if (seq_stage = stg_SED) then
                case seq is
                    when x"00" =>                               -- PH1+: D=1
                        psw_d <= '1';
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- TXS
            if (seq_stage = stg_TXS) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        reg_sp <= reg_x;                        --       store SP=X
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;
            -- TSX
            if (seq_stage = stg_TSX) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        reg_x <= reg_sp;                        --       store X=SP
                        psw_z <= isZero(reg_sp);                --       Z flag
                        psw_n <= reg_sp(7);                     --       N flag
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- PHA
            if (seq_stage = stg_PHA) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        abus <= (x"01" & reg_sp);               --       01:sp to abus
                        abus_off <= '0';
                        dbctl_off <= '0';                       --       dbus to write
                        dbctl_r1w0 <= '0';
                        ALUain <= reg_sp;
                        alu_bin_mode <= bin_set;
                        alu_cin_mode <= cin_clr;
                        alu_din_mode <= din_clr;
                        seq <= countSeq(seq);
                    when x"01" =>                               -- PH1-: (valid addr) sp=sp-1
                        reg_sp <= ALUrout;
                        seq <= countSeq(seq);
                    when x"02" =>                               -- PH2+: place a on dbus
                        outval <= reg_a;
                        seq <= countSeq(seq);
                    when x"03" =>                               -- PH2-: (valid data) instruction done
                        seq_stage <= stg_tail;
                        seq <= x"00";
                    when others => null;
                end case;
            end if;
            -- PHP
            if (seq_stage = stg_PHP) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        abus <= (x"01" & reg_sp);               --       01:sp to abus
                        abus_off <= '0';
                        dbctl_off <= '0';                       --       dbus to write
                        dbctl_r1w0 <= '0';
                        ALUain <= reg_sp;
                        alu_bin_mode <= bin_set;
                        alu_cin_mode <= cin_clr;
                        alu_din_mode <= din_clr;
                        seq <= countSeq(seq);
                    when x"01" =>                               -- PH1-: (valid addr) sp=sp-1
                        reg_sp <= ALUrout;
                        seq <= countSeq(seq);
                    when x"02" =>                               -- PH2+: place a on dbus
                        outval <= (reg_p(7 downto 6) & "01" &
                                   reg_p(3 downto 0));          --       6502 quirk: B always set on pushed psw
                        seq <= countSeq(seq);
                    when x"03" =>                               -- PH2-: (valid data) instruction done
                        seq_stage <= stg_tail;
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- PLA
            if (seq_stage = stg_PLA) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        abus_off <= '0';
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus to read
                        ALUain <= reg_sp;
                        alu_bin_mode <= bin_clr;                --       sp pre-increment
                        alu_cin_mode <= cin_set;
                        alu_din_mode <= din_clr;
                        seq <= countSeq(seq);
                    when x"01" =>                               -- PH1-: (valid addr)
                        reg_sp <= ALUrout;                      --       we didn't have addr ready this PH1+
                        seq <= countSeq(seq);                   --       so we'll need to wait for next PH1+
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" => seq <= countSeq(seq);         -- PH2-: pass
                    when x"04" =>                               -- PH1+: now we have address for bus
                        abus <= (x"01" & reg_sp);               --       01:sp to abus
                        seq <= countSeq(seq);
                    when x"05" => seq <= countSeq(seq);         -- PH1-: (valid addr)
                    when x"06" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"07" =>                               -- PH2-: (valid data) instruction done
                        reg_a <= regbus;
                        psw_z <= isZero(regbus);                --       Z flag
                        psw_n <= regbus(7);                     --       N flag
                        seq_stage <= stg_tail;
                        seq <= x"00";
                    when others => null;
                end case;
            end if;
            -- PLP
            if (seq_stage = stg_PLP) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        abus_off <= '0';
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus to read
                        ALUain <= reg_sp;
                        alu_bin_mode <= bin_clr;                --       sp pre-increment
                        alu_cin_mode <= cin_set;
                        alu_din_mode <= din_clr;
                        seq <= countSeq(seq);
                    when x"01" =>                               -- PH1-: (valid addr)
                        reg_sp <= ALUrout;                      --       we didn't have addr ready this PH1+
                        seq <= countSeq(seq);                   --       so we'll need to wait for next PH1+
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" => seq <= countSeq(seq);         -- PH2-: pass
                    when x"04" =>                               -- PH1+: now we have address for bus
                        abus <= (x"01" & reg_sp);               --       01:sp to abus
                        seq <= countSeq(seq);
                    when x"05" => seq <= countSeq(seq);         -- PH1-: (valid addr)
                    when x"06" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"07" =>                               -- PH2-: (valid data) instruction done
                        reg_p <= (regbus(7 downto 6) & "0" &
                                  regbus(4 downto 0));          --       store status value (unused bit forced to 0)
                        seq_stage <= stg_tail;
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- TAX
            if (seq_stage = stg_TAX) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        reg_x <= reg_a;                         --       store X=A
                        psw_z <= isZero(reg_a);                 --       Z flag
                        psw_n <= reg_a(7);                      --       N flag
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;
            -- TXA
            if (seq_stage = stg_TXA) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        reg_a <= reg_x;                         --       store A=X
                        psw_z <= isZero(reg_x);                 --       Z flag
                        psw_n <= reg_x(7);                      --       N flag
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;
            -- TAY
            if (seq_stage = stg_TAY) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        reg_y <= reg_a;                         --       store Y=A
                        psw_z <= isZero(reg_a);                 --       Z flag
                        psw_n <= reg_a(7);                      --       N flag
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;
            -- TYA
            if (seq_stage = stg_TYA) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        reg_a <= reg_y;                         --       store A=Y
                        psw_z <= isZero(reg_y);                 --       Z flag
                        psw_n <= reg_y(7);                      --       N flag
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- DEX
            if (seq_stage = stg_DEX) then
                case seq is
                    when x"00" =>                               -- PH1+: BUF=x-1
                        buf_data <= dec(reg_x);
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        reg_x <= buf_data;
                        psw_z <= isZero(buf_data);
                        psw_n <= buf_data(7);
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;
            -- DEY
            if (seq_stage = stg_DEY) then
                case seq is
                    when x"00" =>                               -- PH1+: BUF=y-1
                        buf_data <= dec(reg_y);
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        reg_y <= buf_data;
                        psw_z <= isZero(buf_data);
                        psw_n <= buf_data(7);
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;
            -- INX
            if (seq_stage = stg_INX) then
                case seq is
                    when x"00" =>                               -- PH1+: BUF=x+1
                        buf_data <= inc(reg_x);
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        reg_x <= buf_data;
                        psw_z <= isZero(buf_data);
                        psw_n <= buf_data(7);
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;
            -- INY
            if (seq_stage = stg_INY) then
                case seq is
                    when x"00" =>                               -- PH1+: BUF=y+1
                        buf_data <= inc(reg_y);
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        reg_y <= buf_data;
                        psw_z <= isZero(buf_data);
                        psw_n <= buf_data(7);
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- BCC
            if (seq_stage = stg_BCC) then
                case seq is
                    when x"00" =>                               -- PH1+: sync on, PC to abus
                        abus_off <= '0';                        --       BUF=branch offset
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus on for read
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data) store data to BUF
                        buf_data <= regbus;
                        seq <= x"00";
                        if (psw_C = '0') then
                            ret_stage <= stg_reljmp;            -- branch taken
                        else
                            ret_stage <= stg_tail;              -- branch not taken
                        end if;
                        seq_stage <= stg_sub_incpc;
                    when others => null;
                end case;
            end if;

            -- BCS
            if (seq_stage = stg_BCS) then
                case seq is
                    when x"00" =>                               -- PH1+: sync on, PC to abus
                        abus_off <= '0';                        --       BUF=branch offset
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus on for read
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data) store data to BUF
                        buf_data <= regbus;
                        seq <= x"00";
                        if (psw_C = '1') then
                            ret_stage <= stg_reljmp;            --       branch taken
                        else
                            ret_stage <= stg_tail;              --       branch not taken
                        end if;
                        seq_stage <= stg_sub_incpc;             --       ++PC then branch or finish
                    when others => null;
                end case;
            end if;

            -- BNE
            if (seq_stage = stg_BNE) then
                case seq is
                    when x"00" =>                               -- PH1+: sync on, PC to abus
                        abus_off <= '0';                        --       BUF=branch offset
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus on for read
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data) store data to BUF
                        buf_data <= regbus;
                        seq <= x"00";
                        if (psw_Z = '0') then
                            ret_stage <= stg_reljmp;            -- branch taken
                        else
                            ret_stage <= stg_tail;              -- branch not taken
                        end if;
                        seq_stage <= stg_sub_incpc;
                    when others => null;
                end case;
            end if;

            -- BEQ
            if (seq_stage = stg_BEQ) then
                case seq is
                    when x"00" =>                               -- PH1+: sync on, PC to abus
                        abus_off <= '0';                        --       BUF=branch offset
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus on for read
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data) store data to BUF
                        buf_data <= regbus;
                        seq <= x"00";
                        if (psw_Z = '1') then
                            ret_stage <= stg_reljmp;            -- branch taken
                        else
                            ret_stage <= stg_tail;              -- branch not taken
                        end if;
                        seq_stage <= stg_sub_incpc;
                    when others => null;
                end case;
            end if;

            -- BPL
            if (seq_stage = stg_BPL) then
                case seq is
                    when x"00" =>                               -- PH1+: sync on, PC to abus
                        abus_off <= '0';                        --       BUF=branch offset
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus on for read
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data) store data to BUF
                        buf_data <= regbus;
                        seq <= x"00";
                        if (psw_N = '0') then
                            ret_stage <= stg_reljmp;            -- branch taken
                        else
                            ret_stage <= stg_tail;              -- branch not taken
                        end if;
                        seq_stage <= stg_sub_incpc;
                    when others => null;
                end case;
            end if;

            -- BMI
            if (seq_stage = stg_BMI) then
                case seq is
                    when x"00" =>                               -- PH1+: sync on, PC to abus
                        abus_off <= '0';                        --       BUF=branch offset
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus on for read
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data) store data to BUF
                        buf_data <= regbus;
                        seq <= x"00";
                        if (psw_N = '1') then
                            ret_stage <= stg_reljmp;            -- branch taken
                        else
                            ret_stage <= stg_tail;              -- branch not taken
                        end if;
                        seq_stage <= stg_sub_incpc;
                    when others => null;
                end case;
            end if;

            -- BVC
            if (seq_stage = stg_BVC) then
                case seq is
                    when x"00" =>                               -- PH1+: sync on, PC to abus
                        abus_off <= '0';                        --       BUF=branch offset
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus on for read
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data) store data to BUF
                        buf_data <= regbus;
                        seq <= x"00";
                        if (psw_V = '0') then
                            ret_stage <= stg_reljmp;            -- branch taken
                        else
                            ret_stage <= stg_tail;              -- branch not taken
                        end if;
                        seq_stage <= stg_sub_incpc;
                    when others => null;
                end case;
            end if;

            -- BVS
            if (seq_stage = stg_BVS) then
                case seq is
                    when x"00" =>                               -- PH1+: sync on, PC to abus
                        abus_off <= '0';                        --       BUF=branch offset
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus on for read
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data) store data to BUF
                        buf_data <= regbus;
                        seq <= x"00";
                        if (psw_V = '1') then
                            ret_stage <= stg_reljmp;            -- branch taken
                        else
                            ret_stage <= stg_tail;              -- branch not taken
                        end if;
                        seq_stage <= stg_sub_incpc;
                    when others => null;
                end case;
            end if;

            -- ASL A
            if (seq_stage = stg_ASL_a) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        tmp8 := reg_a(6 downto 0) & '0';        --       tmp = A << 1
                        psw_c <= reg_a(7);                      --       C = A(7)
                        psw_z <= isZero(tmp8);                  --       Z flag
                        psw_n <= tmp8(7);                       --       N flag
                        reg_a <= tmp8;                          --       A = tmp
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- ASL [mem]
            if (seq_stage = stg_ASLmem) then
                case seq is
                    when x"00" =>                               -- PH1+: put MEM on abus (read)
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;                       --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data)
                        tmp8 := regbus(6 downto 0) & '0';       --       tmp = [mem] << 1
                        psw_c <= regbus(7);                     --       C = [mem](7)
                        psw_z <= isZero(tmp8);                  --       Z flag
                        psw_n <= tmp8(7);                       --       N flag
                        outval <= tmp8;
                        seq <= countSeq(seq);
                    when x"04" =>                               -- PH1+:
                        dbctl_r1w0 <= '0';                      --       bus to write ([MEM]=tmp)
                    when x"05" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"06" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"07" =>                               -- PH2-: (valid data)
                        seq_stage <= stg_tail;                  --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- LSR A
            if (seq_stage = stg_LSR_a) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        tmp8 := '0' & reg_a(7 downto 1);        --       tmp = A >> 1
                        psw_c <= reg_a(0);                      --       C = A(0)
                        psw_z <= isZero(tmp8);                  --       Z flag
                        psw_n <= tmp8(7);                       --       N flag
                        reg_a <= tmp8;                          --       A = tmp
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- LSR [mem]
            if (seq_stage = stg_LSRmem) then
                case seq is
                    when x"00" =>                               -- PH1+: put MEM on abus (read)
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;                       --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data)
                        tmp8 := '0' & regbus(7 downto 1);     --         tmp = [mem] >> 1
                        psw_c <= regbus(0);                     --       C = [mem](0)
                        psw_z <= isZero(tmp8);                  --       Z flag
                        psw_n <= tmp8(7);                       --       N flag
                        outval <= tmp8;
                        seq <= countSeq(seq);
                    when x"04" =>                               -- PH1+:
                        dbctl_r1w0 <= '0';                      --       bus to write ([MEM]=tmp)
                    when x"05" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"06" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"07" =>                               -- PH2-: (valid data)
                        seq_stage <= stg_tail;                  --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- ROL A
            if (seq_stage = stg_ROL_a) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        tmp8 := reg_a(6 downto 0) & psw_c;      --       tmp = A[6:0],C
                        psw_c <= reg_a(7);                      --       C = A(7)
                        psw_z <= isZero(tmp8);                  --       Z flag
                        psw_n <= tmp8(7);                       --       N flag
                        reg_a <= tmp8;                          --       A = tmp
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- ROL[mem]
            if (seq_stage = stg_ROLmem) then
                case seq is
                    when x"00" =>                               -- PH1+: put MEM on abus (read)
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;                       --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data)
                        tmp8 := regbus(6 downto 0) & psw_c;     --       tmp = [mem](6:0),C
                        psw_c <= regbus(7);                     --       C = [mem](7)
                        psw_z <= isZero(tmp8);                  --       Z flag
                        psw_n <= tmp8(7);                       --       N flag
                        outval <= tmp8;
                        seq <= countSeq(seq);
                    when x"04" =>                               -- PH1+:
                        dbctl_r1w0 <= '0';                      --       bus to write ([MEM]=tmp)
                    when x"05" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"06" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"07" =>                               -- PH2-: (valid data)
                        seq_stage <= stg_tail;                  --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- ROR A
            if (seq_stage = stg_ROR_a) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        tmp8 := psw_c & reg_a(7 downto 1);      --       tmp = C,A[7:1]
                        psw_c <= reg_a(0);                      --       C = A(0)
                        psw_z <= isZero(tmp8);                  --       Z flag
                        psw_n <= tmp8(7);                       --       N flag
                        reg_a <= tmp8;                          --       A = tmp
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>
                        seq_stage <= stg_tail;                  -- PH2-: instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- ROR [mem]
            if (seq_stage = stg_RORmem) then
                case seq is
                    when x"00" =>                               -- PH1+: put MEM on abus (read)
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= buf_addr;                       --       put MEM on abus
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data)
                        tmp8 := psw_c & regbus(7 downto 1);     --       tmp = C,[mem](7:1)
                        psw_c <= regbus(0);                     --       C = [mem](0)
                        psw_z <= isZero(tmp8);                  --       Z flag
                        psw_n <= tmp8(7);                       --       N flag
                        outval <= tmp8;
                        seq <= countSeq(seq);
                    when x"04" =>                               -- PH1+:
                        dbctl_r1w0 <= '0';                      --       bus to write ([MEM]=tmp)
                    when x"05" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"06" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"07" =>                               -- PH2-: (valid data)
                        seq_stage <= stg_tail;                  --       instruction done
                        seq <= x"00";
                    when others => null;
                end case;
            end if;

            -- RTS                                              (PCH=[++SP], PCL=[++SP], incpc, tail)
            if (seq_stage = stg_RTS) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        abus_off <= '0';
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus to read
                        ALUain <= reg_sp;
                        alu_bin_mode <= bin_clr;                --       sp pre-increment
                        alu_cin_mode <= cin_set;
                        alu_din_mode <= din_clr;
                        seq <= countSeq(seq);
                    when x"01" =>                               -- PH1-: (valid addr)
                        buf_data <= ALUrout;                    --       we didn't have addr ready on PH1+
                        seq <= countSeq(seq);                   --       so we'll store this SP+1 in BUF
                    when x"02" =>                               -- PH2+: and pipeline BUF+1 (SP+2)
                        ALUain <= buf_data;                     --       through this bus cycle
                        alu_bin_mode <= bin_clr;
                        alu_cin_mode <= cin_set;
                        seq <= countSeq(seq);
                    when x"03" => seq <= countSeq(seq);         -- PH2-: (valid data)
                        reg_sp <= ALUrout;                      --       SP=SP+2
                        seq <= countSeq(seq);                   --       BUF is PCL, SP is PCH
                    when x"04" =>                               -- PH1+: put out address of PCL
                        abus <= (x"01" & buf_data);             --       01:BUF
                        seq <= countSeq(seq);
                    when x"05" => seq <= countSeq(seq);         -- PH1-: (valid addr)
                    when x"06" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"07" =>                               -- PH2-: (valid data) instruction done
                        reg_pcl <= regbus;
                        seq <= countSeq(seq);
                    when x"08" =>                               -- PH1+: put out address of PCH
                        abus <= (x"01" & reg_sp);               --       01:SP
                        seq <= countSeq(seq);
                    when x"09" => seq <= countSeq(seq);         -- PH1-: (valid addr)
                    when x"0a" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"0b" =>                               -- PH2-: (valid data)
                        reg_pch <= regbus;
                        seq <= countSeq(seq);
                        seq <= x"00";
                        ret_stage <= stg_tail;                  --       JSR saves PC-1 so we incpc
                        seq_stage <= stg_sub_incpc;             --       first, then tail (done)
                    when others => null;
                end case;
            end if;

            -- RTI                                      (P=[++SP], PCH=[++SP], PCL=[++SP], tail)
            if (seq_stage = stg_RTI) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        abus_off <= '0';
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus to read
                        ALUain <= reg_sp;
                        alu_bin_mode <= bin_clr;                --       sp pre-increment
                        alu_cin_mode <= cin_set;
                        alu_din_mode <= din_clr;
                        seq <= countSeq(seq);
                    when x"01" =>                               -- PH1-: (valid addr) not yet available
                        reg_sp <= ALUrout;                      --       ++SP (now on psw)
                        ALUain <= ALUrout;                      --       set up next ++SP
                        seq <= countSeq(seq);                   --       we need to wait for next PH1+
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" => seq <= countSeq(seq);         -- PH2-: pass
                    when x"04" =>                               -- PH1+: now we have addresses for bus
                        abus <= (x"01" & reg_sp);               --       01:sp (psw) to abus
                        reg_sp <= ALUrout;                      --       ++SP (now on PCL)
                        ALUain <= ALUrout;                      --       set up next ++SP
                        seq <= countSeq(seq);
                    when x"05" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"06" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"07" =>                               -- PH2-: (valid data)
                        reg_p <= (regbus(7 downto 6) & "00" &
                                  regbus(3 downto 0));          --       store psw (unused/brk set to 0)
                        seq <= countSeq(seq);
                    when x"08" =>                               -- PH1+:
                        abus <= (x"01" & reg_sp);               --       01:sp (PCL) to abus
                        seq <= countSeq(seq);
                    when x"09" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"0a" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"0b" =>                               -- PH2-: (valid data)
                        reg_pcl <= regbus;                      --       save PCL=[SP]
                        reg_sp <= ALUrout;                      --       ++SP (now on PCH)
                        seq <= countSeq(seq);                   --       BUF is PCL, SP is PCH
                    when x"0c" =>                               -- PH1+: put out address of PCL
                        abus <= (x"01" & reg_sp);               --       01:sp (PCH) to abus
                        seq <= countSeq(seq);
                    when x"0d" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"0e" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"0f" =>                               -- PH2-: (valid data)
                        reg_pch <= regbus;                      --       save PCH=[SP]
                        seq <= x"00";
                        seq_stage <= stg_tail;                  --       instruction done
                    when others => null;
                end case;
            end if;

            -- JSR                                      (buf_addr=[pc].w, ++pc, [sp--]=PCL, [sp--]=PCH)
            if (seq_stage = stg_JSR) then
                case seq is
                    when x"00" =>                               -- PH1+:
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';
                        abus_off <= '0';
                        abus <= reg_pc;
                        ALUain <= reg_pcl;
                        alu_bin_mode <= bin_clr;
                        alu_cin_mode <= cin_set;
                        alu_din_mode <= din_clr;
                        seq <= countSeq(seq);
                    when x"01" =>                               -- PH1-: (valid addr)
                        reg_pcl <= ALUrout;
                        private_c <= ALUcout;                   --       ++PC
                        seq <= countSeq(seq);
                    when x"02" =>                               -- PH2+: pass
                        ALUain <= reg_pch;
                        alu_bin_mode <= bin_clr;
                        alu_cin_mode <= cin_aux;
                        seq <= countSeq(seq);
                    when x"03" =>                               -- PH2-: (valid data) write to MEML
                        buf_addr_l <= regbus;                   --       (we can't store to PC as we're using it)
                        reg_pch <= ALUrout;
                        seq <= countSeq(seq);
                    when x"04" =>                               -- PH1+: put PC (+1) on abus
                        abus <= reg_pc;
                        seq <= countSeq(seq);
                    when x"05" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"06" => seq <= countSeq(seq);         -- PH2+: pass
                        seq <= countSeq(seq);
                    when x"07" =>                               -- PH2-: (valid data) write to MEMH
                        buf_addr_h <= regbus;                   --       (PC not yet saved)
                        seq <= countSeq(seq);
                    when x"08" =>                               -- PH1+:
                        dbctl_r1w0 <= '0';                      --       data bus to write mode                       
                        abus <= (x"01" & reg_sp);               --       put out address of 01:SP
                        ALUain <= reg_sp;                       --       set up SP--
                        alu_bin_mode <= bin_set;
                        alu_cin_mode <= cin_clr;
                        alu_din_mode <= din_clr;
                        seq <= countSeq(seq);
                    when x"09" =>                               -- PH1-: (valid addr)
                        reg_sp <= ALUrout;                      --       SP--
                        seq <= countSeq(seq);
                    when x"0a" => seq <= countSeq(seq);         -- PH2+:
                        outval <= reg_pch;                      --       [01:SP]=PCH
                        seq <= countSeq(seq);
                    when x"0b" =>                               -- PH2-: (valid data) pass
                        seq <= countSeq(seq);
                    when x"0c" =>                               -- PH1+:
                        dbctl_r1w0 <= '0';                      --       data bus to write mode                       
                        abus <= (x"01" & reg_sp);               --       put out address of 01:SP
                        ALUain <= reg_sp;                       --       set up SP--
                        alu_bin_mode <= bin_set;
                        alu_cin_mode <= cin_clr;
                        alu_din_mode <= din_clr;
                        seq <= countSeq(seq);
                    when x"0d" =>                               -- PH1-: (valid addr)
                        reg_sp <= ALUrout;                      --       SP--
                        seq <= countSeq(seq);
                    when x"0e" => seq <= countSeq(seq);         -- PH2+:
                        outval <= reg_pcl;                      --       [01:SP]=PCL
                        seq <= countSeq(seq);
                    when x"0f" =>                               -- PH2-: (valid data)
                        dbctl_r1w0 <= '1';                      --       shut off write
                        reg_pc <= buf_addr;                     --       PC=MEM
                        seq_stage <= stg_tail;
                        seq <= x"00";                           --       instruction done

                    when others => null;
                end case;
            end if;
        end if;

    end process main_proc;

end interaction;
