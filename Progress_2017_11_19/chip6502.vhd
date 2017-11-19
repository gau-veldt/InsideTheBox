----------------------------------------------------------------------------------
--
--  Takes all the VHDL bits and makes a 6502 out of them
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

subtype slv2 is std_logic_vector(1 downto 0);
subtype byte is std_logic_vector(7 downto 0);
subtype word is std_logic_vector(15 downto 0);

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

signal alu_bin_reg : byte;
signal alu_bin_tie : byte;

type stage_t is (
    stg_reset,
    stg_fetch,
    stg_sub_incpc,          -- pc++
    --stg_sub_pc2adr,         -- meml=[pc++], memh=[pc++]
    stg_sub_pc2buf,         -- BUF=[pc++]
    stg_sub_pc2a,           -- A=[pc++]
    stg_sub_pc2x,           -- X=[pc++]
    stg_sub_pc2y,           -- Y=[pc++]
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
    stg_JMP_abs,
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
alias reg_pcl : byte is reg_pc(7 downto 0);
alias reg_pch : byte is reg_pc(15 downto 8);
alias buf_addr_l : byte is buf_addr(7 downto 0);
alias buf_addr_h : byte is buf_addr(15 downto 8);

signal private_c    : std_logic;

function isZero(src: byte) return std_logic is
begin
    return ((src(0) nor src(1)) and (src(2) nor src(3))) and
           ((src(4) nor src(5)) and (src(6) nor src(7)));
end isZero;

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
            
        elsif (rising_edge(ph4Xin)) then

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
    
                                                                    -- LDA
                                when x"A9" =>                       --  imm
                                    ret_stage <= stg_tail;
                                    seq_stage <= stg_sub_pc2a;
    
                                                                    -- LDX
                                when x"A2" =>                       --  imm
                                    ret_stage <= stg_tail;
                                    seq_stage <= stg_sub_pc2x;
                                                                    -- LDY
                                when x"A0" =>                       --  imm
                                    ret_stage <= stg_tail;
                                    seq_stage <= stg_sub_pc2y;
    
                                when x"9A" =>                       --  TXS sp=x
                                    seq_stage <= stg_TXS;
                                when x"BA" =>                       --  TSX x=sp
                                        seq_stage <= stg_TSX;
                                when x"48" =>                       --  PHA [sp--]=a
                                        seq_stage <= stg_PHA;
                                when x"68" =>                       --  PLA a=[++sp]
                                        seq_stage <= stg_PLA;
                                when x"08" =>                       --  PHP [sp--]=P
                                        seq_stage <= stg_PHP;
                                when x"28" =>                       --  PLP P=[++sp]
                                        seq_stage <= stg_PLP;

                                when x"AA" =>                       --  TAX X=A
                                        seq_stage <= stg_TAX;
                                when x"8A" =>                       --  TXA A=X
                                        seq_stage <= stg_TXA;
                                when x"A8" =>                       --  TAY Y=A
                                        seq_stage <= stg_TAY;
                                when x"98" =>                       --  TYA A=Y
                                        seq_stage <= stg_TYA;

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
            -- checks for interrupts, and RDY
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

            if (seq_stage = stg_sub_pc2buf) then
                case seq is
                    when x"00" =>                               -- PH1+: sync on, PC to abus
                        abus_off <= '0';
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus on for read
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data) store data to BUF
                        buf_data <= regbus;
                        seq <= x"00";
                        seq_stage <= stg_sub_incpc;
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_sub_pc2a) then
                case seq is
                    when x"00" =>                               -- PH1+: sync on, PC to abus
                        abus_off <= '0';
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus on for read
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data) store data to A
                        reg_a <= regbus;
                        psw_z <= isZero(regbus);                --       Z flag
                        psw_n <= regbus(7);                     --       N flag
                        seq <= x"00";
                        seq_stage <= stg_sub_incpc;
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_sub_pc2x) then
                case seq is
                    when x"00" =>                               -- PH1+: sync on, PC to abus
                        abus_off <= '0';
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus on for read
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data) store data to X
                        reg_x <= regbus;
                        psw_z <= isZero(regbus);                --       Z flag
                        psw_n <= regbus(7);                     --       N flag
                        seq <= x"00";
                        seq_stage <= stg_sub_incpc;
                    when others => null;
                end case;
            end if;

            if (seq_stage = stg_sub_pc2y) then
                case seq is
                    when x"00" =>                               -- PH1+: sync on, PC to abus
                        abus_off <= '0';
                        abus <= reg_pc;
                        dbctl_off <= '0';
                        dbctl_r1w0 <= '1';                      --       dbus on for read
                        seq <= countSeq(seq);
                    when x"01" => seq <= countSeq(seq);         -- PH1-: (valid addr) pass
                    when x"02" => seq <= countSeq(seq);         -- PH2+: pass
                    when x"03" =>                               -- PH2-: (valid data) store data to Y
                        reg_y <= regbus;
                        psw_z <= isZero(regbus);                --       Z flag
                        psw_n <= regbus(7);                     --       N flag
                        seq <= x"00";
                        seq_stage <= stg_sub_incpc;
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
                        seq_stage <= stg_tail;             --        instruction done
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

        end if;
    end process main_proc;

end interaction;
