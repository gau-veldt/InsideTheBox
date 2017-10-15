----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 07/14/2017 12:34:40 AM
-- Design Name: 
-- Module Name: AudioEcho - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity AudioEcho is
    port (
        led         :   out std_logic_vector(3 downto 0);       -- Pretty light show for debugging stuff
        clk_125     :   in std_logic;                           -- 125 MHz Zybo clock
        ac_sda      :   inout std_logic;                        -- I2C data
        ac_scl      :   inout std_logic                         -- I2C clock
    );
end AudioEcho;

architecture Driver of AudioEcho is
signal led_st : std_logic_vector(3 downto 0) := "0000";         -- output register for leds

subtype tCount is unsigned(31 downto 0);                        -- type alias: 32-bit unsigned integer
constant cZero : tCount := to_unsigned(0,tCount'length);        -- zero constant
constant cMod : tCount := to_unsigned(2,tCount'length);         -- 2/cMod is divider period in seconds
constant cMax : tCount := to_unsigned(125000000,tCount'length); -- input clock frequency constant
signal my_clk : std_logic := '0';                               -- divider output

signal r_sda_i : std_logic := '1';     -- I2C sda  input register
signal r_sda_o : std_logic := '1';     -- I2C sda output register
signal r_scl_i : std_logic := '1';     -- I2C scl  input register
signal r_scl_o : std_logic := '1';     -- I2C scl output register

-- enumeration of audio configuration state machine states
type ac_states is (
    acsInit,
    acsStart,
    acsA7pre,
    acsA7set,
    acsA7out,
    acsA6pre,
    acsA6set,
    acsA6out,
    acsA5pre,
    acsA5set,
    acsA5out,
    acsA4pre,
    acsA4set,
    acsA4out,
    acsA3pre,
    acsA3set,
    acsA3out,
    acsA2pre,
    acsA2set,
    acsA2out,
    acsA1pre,
    acsA1set,
    acsA1out,
    acsWpre,
    acsWset,
    acsWout,
    acsWACKpre,
    acsWACKfloat,
    acsWACKclk,
    acsWACKtest,
    acsStop,
    acsForceStop,
    acsForceStop2,
    acsForceStop3,
    acsSpin
);
signal ac_state : ac_states := acsInit;

signal ac_good : std_logic := '0';

begin

    -- clock divider
    divider: process(clk_125) is
    variable dCur : tCount := cZero;
    begin
        if (rising_edge(clk_125)) then
            dCur:=dCur+cMod;
            if (dCur >= cMax) then
                dCur := dCur - cMax;
                my_clk <= not my_clk;
            end if;
        end if;
    end process divider;

    -- state machine for I2C
    audioconf : process(my_clk) is
    begin
        -- NB: On the Zybo board only two slaves (EEPROM and SSM2603)
        --     are present on the I2C bus thus we are the only master
        --     in the system thus there is no need for arbitration logic.
        if (rising_edge(my_clk)) then
            case ac_state is

                -- Initial state
                when acsInit =>
                        -- Use a counter to pause the minimum initialization
                        -- duration before transitioning to starting condition.
                        ac_state <= acsStart;

                -- Starting condition
                when acsStart =>
                    if (r_scl_i='1' and r_sda_i='1') then
                        -- signal START on the bus
                        r_sda_o<='0';
                        ac_state <= acsA7pre;
                    else
                        -- still waiting for SCL=1 and SDA=1
                        ac_state <= acsStart;
                    end if;

                -- each bit sent takes three states
                -- since for I2C it is required to transition SDA
                -- *after* the SCL low edge and not at the same
                -- time (a device might register SDA before our
                -- clock in that event and misinterpret a STOP
                -- signal if SDA becomes 1 on this cycle when SDA
                -- was 0 on the previous cycle since we'd have
                -- set SCL to 1 to transmit that bit).
                when acsA7pre =>                --  Call SSM2603 address for write (R=1, W=0):
                    r_scl_o<='0';               --  0   0   1   1   0   1   0   W
                    ac_state<=acsA7set;         
                when acsA7set =>               
                    r_sda_o<='0';               --  0
                    ac_state<=acsA7out;
                when acsA7out =>
                    r_scl_o<='1';
                    ac_state <= acsA6pre;

                when acsA6pre =>
                    r_scl_o<='0';
                    ac_state<=acsA6set;
                when acsA6set =>
                    r_sda_o<='0';               --  -   0
                    ac_state<=acsA6out;
                when acsA6out =>
                    r_scl_o<='1';
                    ac_state <= acsA5pre;

                when acsA5pre =>
                    r_scl_o<='0';
                    ac_state<=acsA5set;
                when acsA5set =>
                    r_sda_o<='1';               --  -   -   1
                    ac_state<=acsA5out;
                when acsA5out =>
                    r_scl_o<='1';
                    ac_state <= acsA4pre;

                when acsA4pre =>
                    r_scl_o<='0';
                    ac_state<=acsA4set;
                when acsA4set =>
                    r_sda_o<='1';               --  -   -   -   1
                    ac_state<=acsA4out;
                when acsA4out =>
                    r_scl_o<='1';
                    ac_state <= acsA3pre;

                when acsA3pre =>
                    r_scl_o<='0';
                    ac_state<=acsA3set;
                when acsA3set =>
                    r_sda_o<='0';               --  -   -   -   -   0
                    ac_state<=acsA3out;
                when acsA3out =>
                    r_scl_o<='1';
                    ac_state <= acsA2pre;

                when acsA2pre =>
                    r_scl_o<='0';
                    ac_state<=acsA2set;
                when acsA2set =>
                    r_sda_o<='1';               --  -   -   -   -   -   1
                    ac_state<=acsA2out;
                when acsA2out =>
                    r_scl_o<='1';
                    ac_state <= acsA1pre;

                when acsA1pre =>
                    r_scl_o<='0';
                    ac_state<=acsA1set;
                when acsA1set =>
					-- should be 0 thus we'll be sending the bogus address 0011011 thus
					-- no device will respond so we'll have the negative-acknowledge case
                    r_sda_o<='1';               --  -   -   -   -   -   -   0
                    ac_state<=acsA1out;
                when acsA1out =>
                    r_scl_o<='1';
                    ac_state <= acsWpre;

                when acsWpre =>
                    r_scl_o<='0';
                    ac_state<=acsWset;
                when acsWset =>
                    r_sda_o<='0';               --  -   -   -   -   -   -   -   W
                    ac_state<=acsWout;
                when acsWout =>
                    r_scl_o<='1';
                    ac_state <= acsWACKpre;

                -- On the ackknowledge there's a similar pattern
                -- first we'll lower the clock THEN float SDA
                -- THEN raise the clock THEN test SDA for
                -- ACK/NACK.  THENs mean delay is necessary
                -- thus requiring a different state for each
                -- step so they span different clock cycles
                when acsWACKpre =>
                    r_scl_o<='0';               -- lower clock
                    ac_state<=acsWACKfloat;
                when acsWACKfloat =>
                    r_sda_o<='1';               -- SDA floats to 1
                    ac_state<=acsWACKclk;
                when acsWACKclk =>
                    r_scl_o<='1';               -- raise clock
                    ac_state<=acsWACKtest;
                when acsWACKtest =>
                    -- if all is right SDA should be low at this point
					-- We sent bogus address so it will still be high
					-- this is a negative-acknowledge (NACK).
                    if (r_sda_i='0') then
						-- we won't get here so LED will stay off
                        ac_good<='1';
                    end if;
                    ac_state<=acsForceStop;

                -- forcibly stop:
                -- lower SCL
                -- lower SDA
                -- raise SCL (first three transitions ensure the STOP precondition SCL high, SDA low)
                -- raise SDA (STOP = "transition of SDA from 0 to 1 when SCL is high")
                when acsForceStop =>
                    r_scl_o<='0';
                    ac_state <= acsForceStop2;
                when acsForceStop2 =>
                    r_sda_o<='0';
                    ac_state <= acsForceStop3;
                when acsForceStop3 =>
                    r_scl_o<='1';
                    ac_state <= acsStop;
                when acsStop =>
                    r_sda_o<='1';
                    ac_state <= acsSpin;

                -- Halted (infinite loop)
                when acsSpin =>
                    ac_state <= acsSpin;

                -- Catch-all: enter halted state
                when others =>
                    ac_state <= acsSpin;
            end case;
        end if;
    end process audioconf;

    -- i2c output registers to i2c lines
    with r_sda_o select
        ac_sda <=   '0' when '0',                               -- generate the 0 level (pulls down to ground)
                    'Z' when others;                            -- allows bus pull-up to generate the 1 level
    with r_scl_o select
        ac_scl <=   '0' when '0',                               -- generate the 0 level (pulls down to ground)
                    'Z' when others;                            -- allows bus pull-up to generate the 1 level
    -- i2c lines to i2c input registers
    with ac_sda select
        r_sda_i <=  '0' when '0',
                    '1' when others;
    with ac_scl select
        r_scl_i <=  '0' when '0',
                    '1' when others;

    -- transfer led output register to led output lines
    led_st(0) <= r_sda_i;
    led_st(1) <= r_scl_i;
    led_st(2) <= ac_good;
    led_st(3) <= my_clk; 
    led <= led_st;

end Driver;
