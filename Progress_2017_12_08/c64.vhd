----------------------------------------------------------------------------------
--
--  Commodore 64 on Zybo
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library c64roms;
use c64roms.p_char_rom.all;

entity c64 is
port (
    clk_125     :   in      std_logic;

    -- audio configure
    ac_scl      :   inout   std_logic;
    ac_sda      :   inout   std_logic;

    -- audio signal
    ac_muten    :   out     std_logic;
    ac_mclk     :   out     std_logic;
    ac_bclk     :   out     std_logic;
    ac_pbdat    :   out     std_logic;
    ac_pblrc    :   out     std_logic;
    ac_recdat   :   in      std_logic;
    ac_reclrc   :   in      std_logic;
    -- report error in configuring audio
    led         :   out     std_logic_vector(3 downto 0);
    -- to test waveforms
    sw          :   in      std_logic_vector(3 downto 0);
    -- for freq/wvfm ramping tests
    btn         :   in      std_logic_vector(3 downto 0);

    vga_hs      :   out     std_logic;
    vga_vs      :   out     std_logic;
    vga_r       :   out     std_logic_vector(4 downto 0);
    vga_g       :   out     std_logic_vector(5 downto 0);
    vga_b       :   out     std_logic_vector(4 downto 0)
);
end c64;

architecture c64_guts of c64 is

subtype pair is std_logic_vector(1 downto 0);
subtype slv3 is std_logic_vector(2 downto 0);
subtype nybble is std_logic_vector(3 downto 0);
subtype slv5 is std_logic_vector(4 downto 0);
subtype slv6 is std_logic_vector(5 downto 0);
subtype slv7 is std_logic_vector(6 downto 0);
subtype byte is std_logic_vector(7 downto 0);
subtype ubyte is unsigned(7 downto 0);
subtype slv9 is std_logic_vector(8 downto 0);
subtype slv10 is std_logic_vector(9 downto 0);
subtype slv14 is std_logic_vector(13 downto 0);
subtype word is std_logic_vector(15 downto 0);
subtype u16 is unsigned(15 downto 0);
subtype s16 is signed(15 downto 0);
subtype slv20 is std_logic_vector(19 downto 0);
subtype slv24 is std_logic_vector(23 downto 0);

--
--  Master reset
--
signal res0 : std_logic;
signal res1 : std_logic;                                        -- inverted slave of res0

--
--  Clocks,
--  timing signals,
--  and phase generators
--

--  VIC/CPU clock:
component clk_wiz_0
port
 (-- output clock(s)
  clk160             : out    std_logic;
  -- control signals
  reset              : in     std_logic;
  locked             : out    std_logic;
  -- input clock(s)
  clk_in1            : in     std_logic
 );
end component;

--  SSM2603 I2S audio clock:
component clk_wiz_1
port
 (-- output clock(s)
  clk12              : out    std_logic;
  -- control signals
  reset              : in     std_logic;
  locked             : out    std_logic;
  -- input clock(s)
  clk_in1            : in     std_logic
 );
end component;

signal clk12        : std_logic;
signal clk160       : std_logic;
signal clk_lk1      : std_logic;
signal clk_lk2      : std_logic;
signal clk20_ph1    : std_logic;
signal clk20_ph2    : std_logic;

--  3-bit counter for dividing 160 MHz
--  into 8 20 MHz phases
signal clk20_ph : slv3;
function c20_next(src: slv3) return slv3 is
begin
    case src is
        when "000"  => return "001";
        when "001"  => return "010";
        when "010"  => return "011";
        when "011"  => return "100";
        when "100"  => return "101";
        when "101"  => return "110";
        when "110"  => return "111";
        when "111"  => return "000";
        when others => return "000";
    end case;
end c20_next;

--
--  Memory units and control signals
--

--  64k x 8 main system RAM:
component blk_ram_64k
  port (
    clka : IN STD_LOGIC;
    rsta : IN STD_LOGIC;
    ena : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
  );
end component;
signal ramen        : std_logic := '1';
signal ramdr        : byte;
signal ramdw        : byte;
signal ram_r1w0     : std_logic := '1';
signal ram_r0w1     : std_logic;                                -- inverted slave of ram_r1w0

--  1k x 4 color RAM
component blk_cram
  port (
    clka : IN STD_LOGIC;
    rsta : IN STD_LOGIC;
    ena : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
  );
end component;
signal cramen       : std_logic := '1';
signal cramdr       : nybble;
signal cramdw       : nybble;
signal cram_r1w0    : std_logic := '1';
signal cram_r0w1    : std_logic;                                -- inverted slave of cram_r1w0

signal ram_clk      : std_logic;
signal rama         : word;

--
--  Memory addressing
--
signal bankctl      : slv5 :=  "11111";
alias exrom         : std_logic is bankctl(4);
alias game          : std_logic is bankctl(3);
alias charen        : std_logic is bankctl(2);
alias hiram         : std_logic is bankctl(1);
alias loram         : std_logic is bankctl(0);

type mbank_t is (
    mbk_ram,
    mbk_cram,
    mbk_lorom,
    mbk_hirom,
    mbk_xio2,
    mbk_xio1,
    mbk_cia2,
    mbk_cia1,
    mbk_cgrom,
    mbk_sid,
    mbk_vic
);
signal bank_sel : mbank_t;
function cpu_bank(addr: u16; bsel: slv5) return mbank_t is
variable b3 : slv3;
variable bb0 : std_logic;
variable ce : std_logic;
variable n3 : nybble;
begin
    b3 := bsel(2 downto 0);
    bb0 := bsel(1) nor bsel(0);
    ce := bsel(2);
    n3 := nybble(addr(11 downto 8));
    if (addr >= x"e000") then
        if (bsel(1)='1') then
            return mbk_hirom;
        else
            return mbk_ram;
        end if;
    elsif (addr >= x"d000" and addr < x"e000") then
        if (bb0 = '1') then
            return mbk_ram;
        else
            if (ce = '0') then
                return mbk_cgrom;
            else
                case n3 is
                    when x"f" =>
                        return mbk_xio2;
                    when x"e" =>
                        return mbk_xio1;
                    when x"d" =>
                        return mbk_cia2;
                    when x"c" =>
                        return mbk_cia1;
                    when x"b" =>
                        return mbk_cram;
                    when x"a" =>
                        return mbk_cram;
                    when x"9" =>
                        return mbk_cram;
                    when x"8" =>
                        return mbk_cram;
                    when x"7" =>
                        return mbk_sid;
                    when x"6" =>
                        return mbk_sid;
                    when x"5" =>
                        return mbk_sid;
                    when x"4" =>
                        return mbk_sid;
                    when x"3" =>
                        return mbk_vic;
                    when x"2" =>
                        return mbk_vic;
                    when x"1" =>
                        return mbk_vic;
                    when x"0" =>
                        return mbk_vic;
                    when others =>
                        return mbk_ram;
                end case;                
            end if;
        end if;
    elsif (addr >= x"a000" and addr < x"c000") then
        if ((bsel(1) and bsel(0)) = '1') then
            return mbk_lorom;
        else
            return mbk_ram;
        end if;
    else
        return mbk_ram;
    end if;
end cpu_bank;

--
--  Determine when writes should write to RAM
--
--  Includes any banking configurations that would
--  read RAM or ROM and excludes any that access I/O.
--
signal bank_wt : std_logic;
function bank_is_writethru(addr:word; bsel:slv5) return std_logic is
begin
    case cpu_bank(u16(addr),bsel) is
        when mbk_ram    => return '1';
        when mbk_cram   => return '1';
        when mbk_lorom  => return '1';
        when mbk_hirom  => return '1';
        when mbk_cgrom  => return '1';
        when others     => return '0';
    end case;
end bank_is_writethru;

--
--  6510 processor (CPU)
--
component chip6502 is
port (
    a       :   out     word;
    di      :   in      byte;
    do      :   out     byte;
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
end component;
signal cpu_nmi0     : std_logic:='1';
signal cpu_irq0     : std_logic:='1';
signal cpu_rdy      : std_logic:='1';
signal cpu_so0      : std_logic:='1';
signal cpu_r1w0     : std_logic;
signal cpu_r0w1     : std_logic;                                -- inverted slave of cpu_r1w0
signal cpu_ph1      : std_logic;
signal cpu_ph2      : std_logic;
signal abus         : word;
signal cpu_ioi      : byte;
signal cpu_ioo      : byte;
signal cpudo        : byte;
signal cpudi        : byte;
signal cpu_on       : std_logic;                                -- inverted slave of vic_on
signal cpu_ph4x     : std_logic;

--
--  Video
--
component vic_ii is
port (
    -- register access
    rga         : in      std_logic_vector(5 downto 0);
    rgdi        : in      std_logic_vector(7 downto 0);
    rgdo        : out     std_logic_vector(7 downto 0);
    r1w0        : in      std_logic;
    -- video access
    va          : out     std_logic_vector(13 downto 0);
    vd          : in      std_logic_vector(7 downto 0);
    cd          : in      std_logic_vector(3 downto 0);
    -- bus mastering
    cpu_clk     : out     std_logic;                            -- 4 MHz CPU clock
    cpu_ben     : out     std_logic;                            -- 1=CPU on buses
    vic_ben     : out     std_logic;                            -- 1=VIC on buses
    bus_ph0     : out     std_logic;                            -- master PH0 clock
    bus_ph1     : out     std_logic;                            -- master PH1 clock
    bus_ph2     : out     std_logic;                            -- master PH2 clock
    res0        : in      std_logic;                            -- reset (low)
    -- external signals
    clk20_ph1   : in      std_logic;
    clk20_ph2   : in      std_logic;
    vhs         : out     std_logic;
    vvs         : out     std_logic;
    vr          : out     std_logic_vector(4 downto 0);
    vg          : out     std_logic_vector(5 downto 0);
    vb          : out     std_logic_vector(4 downto 0)
);
end component;


signal vbank        : pair := "00";
signal vic_rga      : slv6;
signal vic_regw     : byte;
signal vic_regr     : byte;
signal vic_r1w0     : std_logic := '1';                         -- write to VIC register
signal vic_va       : slv14;
signal vic_cd       : nybble;
signal vic_vd       : byte;
signal vic_on       : std_logic;
signal cgaddr       : slv12;
signal cgdata       : byte;

--
--  Audio path (SID/SSM2603)
--
-- Initialization data for SSM2603:
function dac_init(lin : ubyte) return slv20 is
begin
    case lin is
        when x"00"      => return x"06" & x"010";
        when x"01"      => return x"02" & x"175";
        when x"02"      => return x"04" & x"010";
        when x"03"      => return x"05" & x"000";
        when x"04"      => return x"07" & x"000";
        when x"05"      => return x"09" & x"001";
        when x"06"      => return x"06" & x"000";
        when others     => return x"ff" & x"fff";
    end case;
end dac_init;

-- I2C xcvr to send initialization data:
component i2c_xcvr port
(
    -- with 1 MHz clock SSM2603 reg 9 delay step is 65.535 ms:
    clk1M       :   in      std_logic;
    
    -- tie to system reset
    res0        :   in      std_logic;
    
    -- initialization line number
    init_line   :   out     unsigned(7 downto 0);
    -- initialization data xRRRRRRRxxxxxxxDDDDDDDDD
    init_data   :   in      std_logic_vector(19 downto 0);
    
    -- error status
    error       :   out     std_logic;
    
    -- tie directly to ac_scl and ac_sda accordingly:
    scl         :   inout   std_logic;
    sda         :   inout   std_logic
);
end component;
signal ssm_init_line    :   ubyte;
signal ssm_init_data    :   slv20;
signal ssm_error        :   std_logic;

-- The 6581 SID chip:
component sid6581 is
port (
    res0        :   in  std_logic;
    ph2         :   in  std_logic;
    rga         :   in  std_logic_vector(4 downto 0);
    din         :   in  std_logic_vector(7 downto 0);
    dout        :   out std_logic_vector(7 downto 0);
    r1w0        :   in  std_logic;
    s16audio    :   out signed(15 downto 0)
);
end component;

signal bclk_cnt : pair;
alias bclk_ref : std_logic is bclk_cnt(0);--1
signal audio_frame : unsigned(4 downto 0);
signal hold_sam : signed(15 downto 0);

signal sid1_rga     : slv5;
signal sid1_dw      : byte;
signal sid1_dr      : byte;
signal sid1_out     : s16;
signal sid1_r1w0    : std_logic;

--
--  When to map CGROM into VIC's address space:
--
function vbk_cgrom(bk: pair; adr: slv14) return std_logic is
variable vsel : slv3;
begin
    vsel := bk(0) & adr(13 downto 12);
    case vsel is
        when "001"  => return '1';
        when others => return '0';
    end case;
end vbk_cgrom;

--
--  For coldstart reset
--
subtype u12 is unsigned(11 downto 0);
signal reset_wait : u12 := x"000";
constant reset_delay : u12 := x"0fb";       -- number of 125 MHz clocks in 2000 ns
signal coldstart : std_logic;

--
--  Architectural implementation
--
begin

--
--  Coldstart reset for 2000 ns
--
initial_reset: process(clk_125, reset_wait) is              -- counts up to 2000 ns
variable not_yet : boolean;
begin
    not_yet := (reset_wait < reset_delay);
    if rising_edge(clk_125) then
        if not_yet then
            reset_wait <= reset_wait + 1;
        end if;
    end if;
    if not_yet then                             -- drive reset accordingly
        coldstart <= '1';
    else
        coldstart <= '0';
    end if;
end process initial_reset;
res0 <= coldstart nor btn(0);

pixclock: clk_wiz_0 port map ( 
    -- Clock out ports  
    clk160 => clk160,
    -- Status and control signals                
    reset => res1,
    locked => clk_lk1,
    -- Clock in ports
    clk_in1 => clk_125
);

clk20gen: process(clk160) is
begin
    if (rising_edge(clk160)) then
        clk20_ph <= c20_next(clk20_ph);
    end if;
end process clk20gen;
clk20_ph1 <= clk20_ph(2) and (clk20_ph(1) nor clk20_ph(0));
clk20_ph2 <= not (clk20_ph(2) or clk20_ph(1) or clk20_ph(0));

ram64: blk_ram_64k port map(
    clka        => ram_clk,
    addra       => rama,
    dina        => ramdw,
    douta       => ramdr,
    wea(0)      => ram_r0w1,
    rsta        => res1,
    ena         => ramen
);

color_ram: blk_cram port map(
    clka        => ram_clk,
    addra       => rama(9 downto 0),
    dina        => cramdw,
    douta       => cramdr,
    wea(0)      => cram_r0w1,
    rsta        => res1,
    ena         => cramen
);

res1 <= not res0;
cpu_r0w1 <= not cpu_r1w0;
ram_r0w1 <= not ram_r1w0;
cram_r0w1 <= not cram_r1w0;
bank_sel <= cpu_bank(u16(abus),bankctl);
bank_wt <= bank_is_writethru(abus,bankctl);

--
--  Determines character generator ROM source address
--
with cpu_on select cgaddr <=
    abus(11 downto 0)       when '1',
    vic_va(11 downto 0)     when others;
cgdata <= char_rom(cgaddr);

--
--  Determine write enables to RAMs
--
cram_wren: process(cpu_r1w0,cpu_on,bank_sel) is
begin
    if (cpu_on='1' and bank_sel=mbk_cram) then
        -- CPU only write to CRAM in CRAM bank
        cram_r1w0       <= cpu_r1w0;
    else
        cram_r1w0       <= '1';         -- VIC never writes
    end if;
end process cram_wren;

ram_wren: process(cpu_r1w0,cpu_on,bank_wt) is
begin
    if (cpu_on='1' and bank_wt='1') then
        -- writes to main RAM or write-thru ranges
        ram_r1w0        <= cpu_r1w0;
    else
        ram_r1w0        <= '1';         -- VIC never writes
    end if;
end process ram_wren;

--  Detemine write enable to VIC
vic_wren: process(cpu_r1w0,cpu_on,bank_sel) is
begin
    if (cpu_on='1' and bank_sel=mbk_vic) then
        vic_r1w0 <= cpu_r1w0;
    else
        vic_r1w0 <= '1';
    end if;
end process vic_wren;

--  Detemine write enable to SID
sid1_wren: process(cpu_r1w0,cpu_on,bank_sel) is
begin
    if (cpu_on='1' and bank_sel=mbk_sid) then
        sid1_r1w0 <= cpu_r1w0;
    else
        sid1_r1w0 <= '1';
    end if;
end process sid1_wren;

--  Only the CPU ever writes data
ramdw       <= cpudo;
cramdw      <= cpudo(3 downto 0);

--
--  Determine where the VIC reads from
--
vic_vd_sel: process(vbank,vic_va,cgdata,ramdr) is
begin
    if (vbk_cgrom(vbank,vic_va) = '1') then
        vic_vd <= cgdata;
    else
        vic_vd <= ramdr;
    end if;
end process vic_vd_sel;
vic_cd      <= cramdr;      --  CRAM has no bank switching

--
--  Determine address given to RAMs
--
ram_a_sel: process(cpu_on,abus,vbank,vic_va) is
begin
    if (cpu_on = '1') then
        rama <= abus;
    else
        rama <= vbank & vic_va;
    end if;
end process ram_a_sel;

--  For clocking RAM read/write cycles
with clk20_ph select ram_clk <=
    '1' when "110",
    '1' when "000",
    '0' when others;

cpu: chip6502 port map (
    nmi0        => cpu_nmi0,
    irq0        => cpu_irq0,
    so0         => '1',
    rdy         => '1',
    a           => abus,
    do          => cpudo,
    di          => cpudi,
    r1w0        => cpu_r1w0,
    pi          => cpu_ioi,
    po          => cpu_ioo,
    ph4Xin      => cpu_ph4x,    
    res0        => res0
);

vic: vic_ii port map(
    clk20_ph1   => clk20_ph1,
    clk20_ph2   => clk20_ph2,
    rga         => vic_rga,
    rgdi        => vic_regw,
    rgdo        => vic_regr,
    r1w0        => vic_r1w0,
    cpu_clk     => cpu_ph4x,
    cpu_ben     => cpu_on,
    vic_ben     => vic_on,
    bus_ph1     => cpu_ph1,
    bus_ph2     => cpu_ph2,
    va          => vic_va,
    vd          => vic_vd,
    cd          => vic_cd,
    res0        => res0,
    vhs         => vga_hs,
    vvs         => vga_vs,
    vr          => vga_r,
    vg          => vga_g,
    vb          => vga_b
);

--  bank selection from CPU IO port
loram       <= cpu_ioo(0);
hiram       <= cpu_ioo(1);
charen      <= cpu_ioo(2);

--  Connect data lines up
vic_regw    <= cpudo;
sid1_dw     <= cpudo;

--  Connect address lines
sid1_rga    <= abus(4 downto 0);
vic_rga     <= abus(5 downto 0);

--  Choose CPU data source (on reads)
with bank_sel select cpudi <=
    ramdr           when mbk_ram,
    x"0" & cramdr   when mbk_cram,
    --              when mbk_lorom,
    --              when mbk_hirom,
    --              when mbk_xio2,
    --              when mbk_xio1,
    --              when mbk_cia2,
    --              when mbk_cia1,
    cgdata          when mbk_cgrom,
    vic_regr        when mbk_sid,
    sid1_dr         when mbk_vic,
    "ZZZZZZZZ"      when others;

--
--  Audio section (including SID)
--
sndclock: clk_wiz_1 port map ( 
    -- Clock out ports  
    clk12 => clk12,
    -- Status and control signals                
    reset => res1,
    locked => clk_lk2,
    -- Clock in ports
    clk_in1 => clk_125
);

sid_1: sid6581 port map (
    res0        => res0,
    ph2         => cpu_ph2,
    rga         => sid1_rga,
    din         => sid1_dw,
    dout        => sid1_dr,
    r1w0        => sid1_r1w0,
    s16audio    => sid1_out
);

ac_muten <= res0;
ac_mclk <= clk12;

bclk_gen : process(clk12,bclk_cnt,res1) is
variable inc_0 : std_logic;
variable inc_1 : std_logic;
begin
    inc_0 := not bclk_cnt(0);
    inc_1 := bclk_cnt(1) xor bclk_cnt(0);
    if (res1 = '1') then
        bclk_cnt <= "00";
    elsif (rising_edge(clk12)) then
        bclk_cnt <= inc_1 & inc_0;
    end if;
end process bclk_gen;

ac_bclk <= bclk_cnt(1);

audio_send : process(bclk_cnt,res1,hold_sam,sid1_out,audio_frame) is
variable apos : nybble;
begin
    -- flipped frame index to send MSB first
    apos := not nybble(audio_frame(3 downto 0));
    if (res1 = '1') then
        audio_frame <= "00000";
    elsif (falling_edge(bclk_cnt(1))) then
        if (audio_frame = "11111") then
            hold_sam <= sid1_out;
        end if;
        ac_pbdat <= hold_sam(to_integer(unsigned(apos)));
        ac_pblrc <= audio_frame(4);
        audio_frame <= audio_frame + 1;
    end if;
end process audio_send;

--
-- I2C config for SSM2603
--
i2c: component i2c_xcvr port map (
    clk1M       => cpu_ph2,
    res0        => res0,
    init_line   => ssm_init_line,
    init_data   => ssm_init_data,
    error       => ssm_error,
    scl         => ac_scl,
    sda         => ac_sda
);
ssm_init_data <= dac_init(ssm_init_line);
led(0) <= ssm_error;

end c64_guts;
