----------------------------------------------------------------------------------
--
--  Generate a single SID voice
--
----------------------------------------------------------------------------------


library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity sid_voice is port (
    clk1M   : in    std_logic;
    freq    : in    unsigned            (15 downto 0);
    pw      : in    unsigned            (11 downto 0);
    wvfm    : in    std_logic_vector    (3 downto 0);
    test    : in    std_logic;
    rmod    : in    std_logic;
    sync    : in    std_logic;
    gate    : in    std_logic;
    enva    : in    unsigned            (3 downto 0);
    envd    : in    unsigned            (3 downto 0);
    envs    : in    unsigned            (3 downto 0);
    envr    : in    unsigned            (3 downto 0);
    envo    : out   unsigned            (7 downto 0);
    osco    : out   unsigned            (7 downto 0);
    uout    : out   unsigned            (7 downto 0)
);
end sid_voice;

architecture sid_voice_impl of sid_voice is

subtype u8      is unsigned             (7 downto 0);
subtype byte    is std_logic_vector     (7 downto 0);
subtype u9      is unsigned             (8 downto 0);
subtype u12     is unsigned             (11 downto 0);
subtype word    is std_logic_vector     (15 downto 0);
subtype u17     is unsigned             (16 downto 0);
subtype u23     is unsigned             (22 downto 0);
subtype u24     is unsigned             (23 downto 0);
subtype word24  is std_logic_vector     (23 downto 0);
subtype dword   is std_logic_vector     (31 downto 0);

signal count24      : u23 := "000" & x"00000";
signal lfsr         : word := x"1337";
signal chg_ph       : std_logic;
signal taps         : std_logic;

signal wv_tri       : u8;
signal wv_saw       : u8;
signal wv_pul       : u8;
signal wv_wht       : u8;
signal wv_raw       : u8;
signal env          : u9;
signal wv_mul       : u17;

-- pipelining
signal wv_raw_reg   : u8;
signal env_reg      : u9;

begin

NCO: process(clk1M, freq, test, count24) is
variable cur : u23;
begin
    if (test='1') then
        --count24 <= x"000000";
        count24 <= "000" & x"00000";
        chg_ph <= '0';
    else
        if (rising_edge(clk1M)) then
            cur := count24;
            cur := cur + freq;
            --chg_ph <= cur(23) xor count24(23);
            chg_ph <= cur(22) xor count24(22);
            count24 <= cur;
        end if;
    end if;
end process NCO;

taps <= ((lfsr(0) xor lfsr(2)) xor lfsr(3)) xor lfsr(5);

noise: process(clk1M,test,chg_ph,taps,lfsr(15 downto 0)) is
begin
    if (test='1') then
        lfsr <= x"1337";
    else
        if (rising_edge(clk1M) and chg_ph='1') then
            lfsr <= taps & lfsr(15 downto 1);
        end if;
    end if;
end process noise;

gen_tri: process(count24) is
variable calc : byte;
begin
    for i in 0 to 6 loop
        --calc(i+1) := count24(16+i) xor count24(23);
        calc(i+1) := count24(15+i) xor count24(22);
    end loop;
    --calc(0) := count24(23);
    calc(0) := count24(22);
    wv_tri <= u8(calc);
end process gen_tri;

gen_saw: process(count24(22 downto 15)) is
begin
    --wv_saw <= count24(23 downto 16);
    wv_saw <= count24(22 downto 15);
end process gen_saw; 

gen_pul: process(count24(22 downto 11),pw) is
variable sample : u12;
begin
    --sample := count24(23 downto 12);
    sample := count24(22 downto 11);
    if ((pw = x"fff") or (sample < pw)) then
        wv_pul <= x"ff";
    else
        wv_pul <= x"00";
    end if;
end process gen_pul;

gen_wht: process(lfsr) is
begin
    wv_wht <= lfsr(15) & lfsr(14) & lfsr(10) & lfsr( 9) &
              lfsr( 5) & lfsr( 4) & lfsr( 3) & lfsr( 0);
end process gen_wht;

-- Handling of waveform selection signals
--
-- The ANDing of multiple waveforms when
-- multiple signals are on is supported
-- (excluding noise).
--
-- We are not replicating the noise-lock bug
-- when noise is combined with other waveforms
-- and c64 programs don't combine noise due to
-- this bug so it's not worthwhile to do so.
with wvfm select wv_raw <=
    x"80"                           when "0000",
    wv_tri                          when "0001",
    wv_saw                          when "0010",
    wv_tri and wv_saw               when "0011",
    wv_pul                          when "0100",
    wv_pul and wv_tri               when "0101",
    wv_pul and wv_saw               when "0110",
    wv_pul and wv_tri and wv_saw    when "0111",
    wv_wht                          when others;

hold_wvraw: process(clk1M,wv_raw) is
begin
    if (rising_edge(clk1M)) then
        wv_raw_reg <= wv_raw;
    end if;
end process hold_wvraw;

-- TODO: envelope generation
env <= '0' & x"ff";

hold_env: process(clk1M,env) is
begin
    if (rising_edge(clk1M)) then
        env_reg <= env+1;
    end if;
end process hold_env;

hold_vmult: process(clk1M, env_reg, wv_raw_reg) is
begin
    if (rising_edge(clk1M)) then
        wv_mul <= env_reg * wv_raw_reg;
    end if;
end process hold_vmult;

uout <= wv_mul(15 downto 8);
envo <= env_reg(7 downto 0);
osco <= wv_raw_reg;

end sid_voice_impl;
