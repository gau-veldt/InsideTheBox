----------------------------------------------------------------------------------
-- 
-- VIC II simulator
--
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity vic_ii is
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
end vic_ii;

architecture vic_ii_impl of vic_ii is

attribute ram_style : string;
attribute gated_clock : string;

--attribute gated_clock of clk20_ph1 : signal is "true";
--attribute gated_clock of clk20_ph2 : signal is "true";

subtype sl is std_logic;
subtype pair is std_logic_vector(1 downto 0);
subtype slv3 is std_logic_vector(2 downto 0);
subtype nybble is std_logic_vector(3 downto 0);
subtype slv6 is std_logic_vector(5 downto 0);
subtype u6 is unsigned(5 downto 0);
subtype byte is std_logic_vector(7 downto 0);
subtype slv9 is std_logic_vector(8 downto 0);
subtype word is std_logic_vector(15 downto 0);
subtype cgptr is std_logic_vector(11 downto 0);
subtype dword is std_logic_vector(31 downto 0);
subtype ubyte is unsigned(7 downto 0);
subtype u16 is unsigned(15 downto 0);
subtype long is unsigned(31 downto 0);
subtype v_addr is std_logic_vector(13 downto 0);
subtype c_addr is std_logic_vector(9 downto 0);
subtype r_addr is std_logic_vector(5 downto 0);

type vregs is array(46 downto 0) of byte;
signal vic_regs : vregs;
attribute ram_style of vic_regs : signal is "registers";

alias vic_M0X       : byte      is vic_regs( 0);
alias vic_M0X8      : sl        is vic_regs(16)(0);
alias vic_M1X       : byte      is vic_regs( 1);
alias vic_M1X8      : sl        is vic_regs(16)(1);
alias vic_M2X       : byte      is vic_regs( 2);
alias vic_M2X8      : sl        is vic_regs(16)(2);
alias vic_M3X       : byte      is vic_regs( 3);
alias vic_M3X8      : sl        is vic_regs(16)(3);
alias vic_M4X       : byte      is vic_regs( 4);
alias vic_M4X8      : sl        is vic_regs(16)(4);
alias vic_M5X       : byte      is vic_regs( 5);
alias vic_M5X8      : sl        is vic_regs(16)(5);
alias vic_M6X       : byte      is vic_regs( 6);
alias vic_M6X8      : sl        is vic_regs(16)(6);
alias vic_M7X       : byte      is vic_regs( 7);
alias vic_M7X8      : sl        is vic_regs(16)(7);
alias vic_M0Y       : byte      is vic_regs( 8);
alias vic_M1Y       : byte      is vic_regs( 9);
alias vic_M2Y       : byte      is vic_regs(10);
alias vic_M3Y       : byte      is vic_regs(11);
alias vic_M4Y       : byte      is vic_regs(12);
alias vic_M5Y       : byte      is vic_regs(13);
alias vic_M6Y       : byte      is vic_regs(14);
alias vic_M7Y       : byte      is vic_regs(15);
alias vic_YSCROLL   : slv3      is vic_regs(17)(2 downto 0);
alias vic_RSEL      : sl        is vic_regs(17)(3);
alias vic_DEN       : sl        is vic_regs(17)(4);
alias vic_BMM       : sl        is vic_regs(17)(5);
alias vic_ECM       : sl        is vic_regs(17)(6);
alias vic_RST8      : sl        is vic_regs(17)(7);
alias vic_RASTER    : byte      is vic_regs(18);
alias vic_LPX       : byte      is vic_regs(19);
alias vic_LPY       : byte      is vic_regs(20);
alias vic_M0E       : sl        is vic_regs(21)(0);
alias vic_M1E       : sl        is vic_regs(21)(1);
alias vic_M2E       : sl        is vic_regs(21)(2);
alias vic_M3E       : sl        is vic_regs(21)(3);
alias vic_M4E       : sl        is vic_regs(21)(4);
alias vic_M5E       : sl        is vic_regs(21)(5);
alias vic_M6E       : sl        is vic_regs(21)(6);
alias vic_M7E       : sl        is vic_regs(21)(7);
alias vic_XSCROLL   : slv3      is vic_regs(22)(2 downto 0);
alias vic_CSEL      : sl        is vic_regs(22)(3);
alias vic_MCM       : sl        is vic_regs(22)(4);
alias vic_RES       : sl        is vic_regs(22)(5);
alias vic_M0YE      : sl        is vic_regs(23)(0);
alias vic_M1YE      : sl        is vic_regs(23)(1);
alias vic_M2YE      : sl        is vic_regs(23)(2);
alias vic_M3YE      : sl        is vic_regs(23)(3);
alias vic_M4YE      : sl        is vic_regs(23)(4);
alias vic_M5YE      : sl        is vic_regs(23)(5);
alias vic_M6YE      : sl        is vic_regs(23)(6);
alias vic_M7YE      : sl        is vic_regs(23)(7);
alias vic_CB        : slv3      is vic_regs(24)(3 downto 1);
alias vic_VM        : nybble    is vic_regs(24)(7 downto 4);
alias vic_IRQ       : sl        is vic_regs(25)(7);
alias vic_ILP       : sl        is vic_regs(25)(3);
alias vic_IMMC      : sl        is vic_regs(25)(2);
alias vic_IMBC      : sl        is vic_regs(25)(1);
alias vic_IRST      : sl        is vic_regs(25)(0);
alias vic_ELP       : sl        is vic_regs(26)(3);
alias vic_EMMC      : sl        is vic_regs(26)(2);
alias vic_EMBC      : sl        is vic_regs(26)(1);
alias vic_ERST      : sl        is vic_regs(26)(0);
alias vic_M0DP      : sl        is vic_regs(27)(0);
alias vic_M1DP      : sl        is vic_regs(27)(1);
alias vic_M2DP      : sl        is vic_regs(27)(2);
alias vic_M3DP      : sl        is vic_regs(27)(3);
alias vic_M4DP      : sl        is vic_regs(27)(4);
alias vic_M5DP      : sl        is vic_regs(27)(5);
alias vic_M6DP      : sl        is vic_regs(27)(6);
alias vic_M7DP      : sl        is vic_regs(27)(7);
alias vic_M0MC      : sl        is vic_regs(28)(0);
alias vic_M1MC      : sl        is vic_regs(28)(1);
alias vic_M2MC      : sl        is vic_regs(28)(2);
alias vic_M3MC      : sl        is vic_regs(28)(3);
alias vic_M4MC      : sl        is vic_regs(28)(4);
alias vic_M5MC      : sl        is vic_regs(28)(5);
alias vic_M6MC      : sl        is vic_regs(28)(6);
alias vic_M7MC      : sl        is vic_regs(28)(7);
alias vic_M0XE      : sl        is vic_regs(29)(0);
alias vic_M1XE      : sl        is vic_regs(29)(1);
alias vic_M2XE      : sl        is vic_regs(29)(2);
alias vic_M3XE      : sl        is vic_regs(29)(3);
alias vic_M4XE      : sl        is vic_regs(29)(4);
alias vic_M5XE      : sl        is vic_regs(29)(5);
alias vic_M6XE      : sl        is vic_regs(29)(6);
alias vic_M7XE      : sl        is vic_regs(29)(7);
alias vic_M0M       : sl        is vic_regs(30)(0);
alias vic_M1M       : sl        is vic_regs(30)(1);
alias vic_M2M       : sl        is vic_regs(30)(2);
alias vic_M3M       : sl        is vic_regs(30)(3);
alias vic_M4M       : sl        is vic_regs(30)(4);
alias vic_M5M       : sl        is vic_regs(30)(5);
alias vic_M6M       : sl        is vic_regs(30)(6);
alias vic_M7M       : sl        is vic_regs(30)(7);
alias vic_M0D       : sl        is vic_regs(31)(0);
alias vic_M1D       : sl        is vic_regs(31)(1);
alias vic_M2D       : sl        is vic_regs(31)(2);
alias vic_M3D       : sl        is vic_regs(31)(3);
alias vic_M4D       : sl        is vic_regs(31)(4);
alias vic_M5D       : sl        is vic_regs(31)(5);
alias vic_M6D       : sl        is vic_regs(31)(6);
alias vic_M7D       : sl        is vic_regs(31)(7);
alias vic_EC        : nybble    is vic_regs(32)(3 downto 0);
alias vic_B0C       : nybble    is vic_regs(33)(3 downto 0);
alias vic_B1C       : nybble    is vic_regs(34)(3 downto 0);
alias vic_B2C       : nybble    is vic_regs(35)(3 downto 0);
alias vic_B3C       : nybble    is vic_regs(36)(3 downto 0);
alias vic_MM0       : nybble    is vic_regs(37)(3 downto 0);
alias vic_MM1       : nybble    is vic_regs(38)(3 downto 0);
alias vic_M0C       : nybble    is vic_regs(39)(3 downto 0);
alias vic_M1C       : nybble    is vic_regs(40)(3 downto 0);
alias vic_M2C       : nybble    is vic_regs(41)(3 downto 0);
alias vic_M3C       : nybble    is vic_regs(42)(3 downto 0);
alias vic_M4C       : nybble    is vic_regs(43)(3 downto 0);
alias vic_M5C       : nybble    is vic_regs(44)(3 downto 0);
alias vic_M6C       : nybble    is vic_regs(45)(3 downto 0);
alias vic_M7C       : nybble    is vic_regs(46)(3 downto 0);

type clist is array(15 downto 0) of byte;

constant vc_red : clist := (
     0=>x"00",  1=>x"FF",  2=>x"a1",  3=>x"6a",  4=>x"a2",  5=>x"5c",  6=>x"50",  7=>x"cb",
     8=>x"a3",  9=>x"6e", 10=>x"cc", 11=>x"63", 12=>x"8b", 13=>x"9b", 14=>x"8a", 15=>x"af"
);
constant vc_green : clist := (
     0=>x"00",  1=>x"FF",  2=>x"4d",  3=>x"c1",  4=>x"57",  5=>x"ad",  6=>x"44",  7=>x"d6",
     8=>x"68",  9=>x"53", 10=>x"7f", 11=>x"63", 12=>x"8b", 13=>x"e3", 14=>x"7f", 15=>x"af"
);
constant vc_blue : clist := (
     0=>x"00",  1=>x"FF",  2=>x"43",  3=>x"c8",  4=>x"a5",  5=>x"5f",  6=>x"9c",  7=>x"89",
     8=>x"3a",  9=>x"0b", 10=>x"76", 11=>x"63", 12=>x"8b", 13=>x"9d", 14=>x"cd", 15=>x"af"
);

function vcolor_r(color : nybble) return byte is
    begin return vc_red(to_integer(unsigned(color))); end vcolor_r;

function vcolor_g(color : nybble) return byte is
    begin return vc_green(to_integer(unsigned(color))); end vcolor_g;

function vcolor_b(color : nybble) return byte is
    begin return vc_blue(to_integer(unsigned(color))); end vcolor_b;

function count3(org: slv3) return slv3 is
begin
    case org is
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
end count3;

function clk20ph_count(org: u6) return u6 is
begin
    case org is
        when "000000"  => return "000001";
        when "000001"  => return "000010";
        when "000010"  => return "000011";
        when "000011"  => return "000100";
        when "000100"  => return "000101";
        when "000101"  => return "000110";
        when "000110"  => return "000111";
        when "000111"  => return "001000";
        when "001000"  => return "001001";
        when "001001"  => return "010000";
        when "010000"  => return "010001";
        when "010001"  => return "010010";
        when "010010"  => return "010011";
        when "010011"  => return "010100";
        when "010100"  => return "010101";
        when "010101"  => return "010110";
        when "010110"  => return "010111";
        when "010111"  => return "011000";
        when "011000"  => return "011001";
        when "011001"  => return "100000";
        when "100000"  => return "100001";
        when "100001"  => return "100010";
        when "100010"  => return "100011";
        when "100011"  => return "100100";
        when "100100"  => return "100101";
        when "100101"  => return "100110";
        when "100110"  => return "100111";
        when "100111"  => return "101000";
        when "101000"  => return "101001";
        when "101001"  => return "110000";
        when "110000"  => return "110001";
        when "110001"  => return "110010";
        when "110010"  => return "110011";
        when "110011"  => return "110100";
        when "110100"  => return "110101";
        when "110101"  => return "110110";
        when "110110"  => return "110111";
        when "110111"  => return "111000";
        when "111000"  => return "111001";
        when "111001"  => return "000000";
        when others => return "000000";
    end case;
end clk20ph_count;

signal clk20ph      : u6 := "001001";
signal clk20stg     : pair;
signal clk20tik     : nybble := "0000";
signal cpuclk       : std_logic;
signal ph0          : std_logic;
signal ph1          : std_logic;
signal ph2          : std_logic;
signal cpu_slice    : std_logic;
signal vic_slice    : std_logic;

signal rapos_V  : u16 := "0000000000000000";
signal rapos_H  : u16 := "0000000000000000";
signal vis_V    : u16;
signal vis_H    : u16;
signal win_V    : u16;
signal win_H    : u16;
signal vbdr     : std_logic;
signal hbdr     : std_logic;
signal bdr      : std_logic;
signal en_V     : std_logic;
signal en_H     : std_logic;
signal en       : std_logic;
signal cell_h   : ubyte;
signal cell_v   : ubyte;
signal cell_ph  : ubyte;
signal cell_pv  : ubyte;
signal rg_o     : byte;
signal rg_i     : byte;

function d_to_slv(arg : long) return dword is
begin
    return dword(arg);
end d_to_slv;

-- line pixel/color registers
subtype slin is std_logic_vector(23 downto 0);
subtype sptr_t is std_logic_vector(13 downto 0);
type cline is array(39 downto 0) of nybble;
type bline is array(39 downto 0) of byte;
type sptr is array(7 downto 0) of sptr_t;
type sdata is array(7 downto 0) of slin;
signal line_pix     : std_logic_vector(319 downto 0) := (others => '0');    -- pixels
signal line_c       : cline := (others => x"f");                            -- color
signal line_b       : bline := (others => x"ff");                           -- block (character)
signal line_sprs    : sdata := (others => x"000000");                       -- sprites
signal spr_ptr      : sptr  := (others => "00000000000000");

function u8toi(src: ubyte) return integer is
begin
    return to_integer(unsigned(src));
end u8toi;

function ratoi(src: slv6) return integer is
begin
    return to_integer(unsigned(src));
end ratoi;

function pxtoi(src: slv9) return integer is
begin
    return to_integer(unsigned(src));
end pxtoi;

function inc6(src: slv6) return slv6 is
variable conv : unsigned(5 downto 0);
begin
    conv := unsigned(src) + 1;
    return slv6(conv);
end inc6;

function cpu_read(cbus,cr1w0 : std_logic) return boolean is
begin
    case cbus and cr1w0 is
        when '1'        => return true;
        when others     => return false;
    end case;
end;

function cpu_write(cbus,cr1w0 : std_logic) return boolean is
begin
    case cbus and (not cr1w0) is
        when '1'        => return true;
        when others     => return false;
    end case;
end;

function reg_in_range(rega: slv6) return boolean is
begin
    case rega is
        when "000000"   => return true;
        when "000001"   => return true;
        when "000010"   => return true;
        when "000011"   => return true;
        when "000100"   => return true;
        when "000101"   => return true;
        when "000110"   => return true;
        when "000111"   => return true;
        when "001000"   => return true;
        when "001001"   => return true;
        when "001010"   => return true;
        when "001011"   => return true;
        when "001100"   => return true;
        when "001101"   => return true;
        when "001110"   => return true;
        when "001111"   => return true;
        when "010000"   => return true;
        when "010001"   => return true;
        when "010010"   => return true;
        when "010011"   => return true;
        when "010100"   => return true;
        when "010101"   => return true;
        when "010110"   => return true;
        when "010111"   => return true;
        when "011000"   => return true;
        when "011001"   => return true;
        when "011010"   => return true;
        when "011011"   => return true;
        when "011100"   => return true;
        when "011101"   => return true;
        when "011110"   => return true;
        when "011111"   => return true;
        when "100000"   => return true;
        when "100001"   => return true;
        when "100010"   => return true;
        when "100011"   => return true;
        when "100100"   => return true;
        when "100101"   => return true;
        when "100110"   => return true;
        when "100111"   => return true;
        when "101000"   => return true;
        when "101001"   => return true;
        when "101010"   => return true;
        when "101011"   => return true;
        when "101100"   => return true;
        when "101101"   => return true;
        when "101110"   => return true;
        when others     => return false;
    end case;
end reg_in_range;

type fstg_t is (
    get_idle,
    get_m_ptrs,
    get_m_data,
    get_c_ptrs,
    get_c_data
);
signal fetch_stg    : fstg_t := get_idle;
signal fetch_m      : slv3 := "000";
signal fetch_n      : slv6 := "000000";
signal fetch_p      : u16;

--
-- "unused vic register bits yield 1 on reading"
--
impure function vic_regs_masked(reg: integer) return byte is
begin
    case reg is
        when 22     => return ("11" & vic_regs(reg)(5 downto 0));
        when 24     => return (vic_regs(reg)(7 downto 1) & '1');
        when 25     => return (vic_regs(reg)(7) & "111" & vic_regs(reg)(3 downto 0));
        when 26     => return ("1111" & vic_regs(reg)(3 downto 0));
        when 32     => return ("1111" & vic_regs(reg)(3 downto 0));
        when 33     => return ("1111" & vic_regs(reg)(3 downto 0));
        when 34     => return ("1111" & vic_regs(reg)(3 downto 0));
        when 35     => return ("1111" & vic_regs(reg)(3 downto 0));
        when 36     => return ("1111" & vic_regs(reg)(3 downto 0));
        when 37     => return ("1111" & vic_regs(reg)(3 downto 0));
        when 38     => return ("1111" & vic_regs(reg)(3 downto 0));
        when 39     => return ("1111" & vic_regs(reg)(3 downto 0));
        when 40     => return ("1111" & vic_regs(reg)(3 downto 0));
        when 41     => return ("1111" & vic_regs(reg)(3 downto 0));
        when 42     => return ("1111" & vic_regs(reg)(3 downto 0));
        when 43     => return ("1111" & vic_regs(reg)(3 downto 0));
        when 44     => return ("1111" & vic_regs(reg)(3 downto 0));
        when 45     => return ("1111" & vic_regs(reg)(3 downto 0));
        when 46     => return ("1111" & vic_regs(reg)(3 downto 0));
        when others => return vic_regs(reg);
    end case;
end vic_regs_masked;

begin

clock20ph: process(clk20_ph1,clk20_ph2,clk20ph,rapos_V,rapos_H,res0) is
variable raV : u16;
variable raH : u16;
begin
    raV := rapos_V;
    raH := rapos_H;
    if (res0 = '0') then
        clk20ph         <= "001001";
        rapos_V         <= "0000000000000000";
        rapos_H         <= "0000000000000000";
    else
        if (falling_edge(clk20_ph2)) then
        --if (rising_edge(clk20_ph2)) then
            clk20ph <= clk20ph_count(clk20ph);
            raV := rapos_V;
            raH := raH + 1;
            if (raH >= 528) then
                raH := "0000000000000000";
                raV := raV + 1;
                if (raV >= 628) then
                    raV := "0000000000000000";
                end if;
            end if;
            rapos_V <= raV;
            rapos_H <= raH;
        end if;
        if (falling_edge(clk20_ph1)) then
        --if (rising_edge(clk20_ph1)) then
            clk20ph <= clk20ph_count(clk20ph);
        end if;
    end if;
end process clock20ph;

cpu_clocks: process(clk20_ph1,clk20_ph2,clk20ph) is
begin
    if (rising_edge(clk20_ph1) or rising_edge(clk20_ph2)) then
        case clk20ph is
            when "110001"   => cpuclk <= '1';
            when "110011"   => cpuclk <= '1';
            when "110101"   => cpuclk <= '1';
            when "110111"   => cpuclk <= '1';
            when others     => cpuclk <= '0';
        end case;
        case clk20ph is
            when "110001"   => ph0 <= '1';
            when "110010"   => ph0 <= '1';
            when "110011"   => ph0 <= '1';
            when "110100"   => ph0 <= '1';
            when others     => ph0 <= '0';
        end case;
        case clk20ph is
            when "110001"   => ph1 <= '1';
            when others     => ph1 <= '0';
        end case;
        case clk20ph is
            when "110101"   => ph2 <= '1';
            when others     => ph2 <= '0';
        end case;
    end if;
end process cpu_clocks;

cpu_clk <= cpuclk;
bus_ph0 <= ph0;
bus_ph1 <= ph1;
bus_ph2 <= ph2;

clk20stg <= pair(clk20ph(5 downto 4));
cpu_slice <= clk20stg(1) and clk20stg(0);           -- when 1 it's CPU's turn on bus
vic_slice <= clk20stg(1) nand clk20stg(0);          -- when 1 it's VIC's turn on bus
cpu_ben <= cpu_slice;
vic_ben <= vic_slice;

poscalc: process(clk20_ph2, rapos_V, rapos_H) is
begin
    vis_V <= rapos_V - 5;
    --win_V <= rapos_V - 105;
    case vic_YSCROLL is
        when "000"  => win_V <= rapos_V -  99;
        when "001"  => win_V <= rapos_V - 101;
        when "010"  => win_V <= rapos_V - 103;
        when "011"  => win_V <= rapos_V - 105;
        when "100"  => win_V <= rapos_V - 107;
        when "101"  => win_V <= rapos_V - 109;
        when "110"  => win_V <= rapos_V - 111;
        when others => win_V <= rapos_V - 113;
    end case;

    vis_H <= rapos_H - 84;
    --win_H <= rapos_H - 124;
    case vic_XSCROLL is
        when "000"  => win_H <= rapos_H - 125;
        when "001"  => win_H <= rapos_H - 126;
        when "010"  => win_H <= rapos_H - 127;
        when "011"  => win_H <= rapos_H - 128;
        when "100"  => win_H <= rapos_H - 129;
        when "101"  => win_H <= rapos_H - 130;
        when "110"  => win_H <= rapos_H - 131;
        when others => win_H <= rapos_H - 132;
    end case;

end process poscalc;

cell_H <= ubyte(win_H(10 downto 3));
cell_V <= ubyte(win_V(11 downto 4));            -- ignoring bit 0 (so odd lines repeat the even lines)          
cell_PH <= ubyte("00000" & win_H(2 downto 0));
cell_PV <= ubyte("00000" & win_V(3 downto 1));  -- ignoring bit 0 (so odd lines repeat the even lines)

hb_calc: process(clk20_ph2, rapos_H, vic_CSEL) is
begin
    if (falling_edge(clk20_ph2)) then
        if    (vic_CSEL='1' and ((rapos_H < 124) or (rapos_H > 443))) then
            hbdr <= '1';
        elsif (vic_CSEL='0' and ((rapos_H < 131) or (rapos_H > 434))) then
            hbdr <= '1';
        else
            hbdr <= '0';
        end if;
    end if;
end process hb_calc;

vb_calc: process(clk20_ph2, rapos_V, vic_RSEL) is
begin
    if (falling_edge(clk20_ph2)) then
        if    (vic_RSEL='1' and ((rapos_V < 105) or (rapos_V > 504))) then
            vbdr <= '1';
        elsif (vic_RSEL='0' and ((rapos_V < 113) or (rapos_V > 496))) then
            vbdr <= '1';
        else
            vbdr <= '0';
        end if;
    end if;
end process vb_calc;

eh_calc: process(clk20_ph2, rapos_H) is
begin
    if (falling_edge(clk20_ph2)) then
        if ((rapos_H < 84) or (rapos_H > 483)) then
            en_H <= '0';
        else
            en_H <= '1';
        end if;
    end if;
end process eh_calc;

ev_calc: process(clk20_ph2, rapos_V) is
begin
    if (falling_edge(clk20_ph2)) then
        if ((rapos_V < 5) or (rapos_V > 604)) then
            en_V <= '0';
        else
            en_V <= '1';
        end if;
    end if;
end process ev_calc;

bdr <= vbdr or hbdr;
en <= en_V and en_H;

hsync: process(rapos_H) is
begin
    if (rapos_H < 64) then
        vhs <= '1';
    else
        vhs <= '0';
    end if;
end process hsync;

vsync: process(rapos_V) is
begin
    if (rapos_V < 4) then
        vvs <= '1';
    else
        vvs <= '0';
    end if;
end process vsync;

rgdo <= rg_o;
rg_i <= rgdi;

vreg_rd: process(ph2,cpu_slice,rga,vic_regs,r1w0) is
begin
    if (rising_edge(ph2)) then
        if (cpu_read(cpu_slice,r1w0)) then
            -- reading register
            if (reg_in_range(rga)) then
                --rg_o <= vic_regs(ratoi(rga));
                rg_o <= vic_regs_masked(ratoi(rga));
            else
                rg_o <= x"FF";
            end if;
        end if;
    end if;
end process vreg_rd;

vreg_wr: process(ph2,cpu_slice,rga,r1w0,rg_i) is
begin
    if (falling_edge(ph2)) then
        if  (cpu_write(cpu_slice,r1w0) and reg_in_range(rga)) then
            -- writing register
            vic_regs(ratoi(rga)) <= rg_i;
        end if;
    end if;
end process vreg_wr;

fetching: process(clk20_ph1,clk20_ph2,vic_slice,fetch_stg,fetch_m,cell_PV,cd,vd,
                  win_v,res0,rapos_H,fetch_n,fetch_p,vic_VM,vic_CB,line_c,line_b) is
variable cur_stg : fstg_t;
variable cur_n : slv6;
begin
    if (res0 = '0') then
        fetch_stg <= get_idle;
    else
        if (vic_slice='1') then
            if (rising_edge(clk20_ph1)) then
                case fetch_stg is
                    when get_idle =>
                        va <= "11111111111111";
                    when get_m_ptrs =>
                        va <= vic_VM & c_addr(fetch_p(9 downto 0));
                    when get_m_data =>
                        va <= vic_CB & line_b(ratoi(fetch_n)) & slv3(cell_PV(2 downto 0));
                    when others =>
                        null;
                end case;
            end if;
            if (falling_edge(clk20_ph2)) then
                cur_stg := fetch_stg;
                cur_n := fetch_n;
                case cur_stg is
                    when get_idle =>
                        if (rapos_H < x"0018" and win_V < x"8000") then
                            if (win_V(3 downto 0) = "0000") then
                                fetch_stg <= get_m_ptrs;
                                fetch_m <= "000";
                                fetch_n <= "000000";
                                fetch_p <= (cell_V*40);
                            else
                                fetch_stg <= get_m_data;
                                fetch_m <= "000";
                                fetch_n <= "000000";
                            end if;
                        end if;
                    when get_m_ptrs =>
                        line_c(ratoi(cur_n)) <= cd;
                        line_b(ratoi(cur_n)) <= vd;
                        if (cur_n < "100111") then
                            fetch_n <= inc6(cur_n);
                            fetch_p <= fetch_p + 1;
                        else
                            fetch_n <= "000000";
                            fetch_stg <= get_m_data;
                        end if;
                    when get_m_data =>
                        line_pix(pxtoi(fetch_n & "000")) <= vd(0);
                        line_pix(pxtoi(fetch_n & "001")) <= vd(1);
                        line_pix(pxtoi(fetch_n & "010")) <= vd(2);
                        line_pix(pxtoi(fetch_n & "011")) <= vd(3);
                        line_pix(pxtoi(fetch_n & "100")) <= vd(4);
                        line_pix(pxtoi(fetch_n & "101")) <= vd(5);
                        line_pix(pxtoi(fetch_n & "110")) <= vd(6);
                        line_pix(pxtoi(fetch_n & "111")) <= vd(7);
                        if (fetch_n < "100111") then
                            fetch_n <= inc6(fetch_n);
                        else
                            fetch_n <= "000000";
                            fetch_stg <= get_idle;
                        end if;
                    when others =>
                        null;
                end case;
            end if;
        end if;
    end if;
end process fetching;

pixgen: process(en,bdr,vic_EC,win_H,cell_H,vic_B0C,clk20_ph1) is
variable cur_px_pos : slv9;
begin
    cur_px_pos := "000000000";
    if (rising_edge(clk20_ph1)) then
        if (en = '1') then
            if (bdr = '1') then
                vr <= vcolor_r(vic_EC)(7 downto 3);
                vg <= vcolor_g(vic_EC)(7 downto 2);
                vb <= vcolor_b(vic_EC)(7 downto 3);
            else
                cur_px_pos(8 downto 3) := slv6(win_H(8 downto 3));
                cur_px_pos(2 downto 0) := "111" xor slv3(win_H(2 downto 0));
                if (line_pix(to_integer(unsigned(cur_px_pos))) = '1') then
                    vr <= vcolor_r(line_c(u8toi(cell_H)))(7 downto 3);
                    vg <= vcolor_g(line_c(u8toi(cell_H)))(7 downto 2);
                    vb <= vcolor_b(line_c(u8toi(cell_H)))(7 downto 3);
                else
                    vr <= vcolor_r(vic_B0C)(7 downto 3);
                    vg <= vcolor_g(vic_B0C)(7 downto 2);
                    vb <= vcolor_b(vic_B0C)(7 downto 3);
                end if;
            end if;
        else
            vr <= "00000";
            vg <= "000000";
            vb <= "00000";
        end if;
    end if;
end process pixgen;

end vic_ii_impl;
