----------------------------------------------------------------------------------
--
--  I2C Transceiver
--
--  Used to send SSM2603 initialization data via I2C
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity i2c_xcvr is
port (
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
end i2c_xcvr;

architecture i2c_init_guts of i2c_xcvr is

subtype     byte        is  std_logic_vector(7 downto 0);
subtype     ubyte       is  unsigned(7 downto 0);
subtype     u16         is  unsigned(15 downto 0);
subtype     slv20       is  std_logic_vector(19 downto 0);
subtype     slv27       is  std_logic_vector(26 downto 0);

constant    dac_addr_w : byte       := "0011010" & "0";
constant    dac_addr_r : byte       := "0011010" & "1";
signal      dac_buffer : slv27      := x"00" & '1' & x"00" & '1' & x"00" & '1'; --padded for ACKs
signal      dac_x_offs : ubyte      := x"00";
signal      dac_init_line : ubyte   := x"00";
signal      dac_init_wait : u16     := x"0000";
signal      dac_ok : std_logic      := '1';
type dacstg_t is (
    dac_reset,
    dac_i2c_start,
    dac_loadbuf,
    dac_i2c_send0_c0,
    dac_i2c_send1_c0dx,
    dac_i2c_send2_c1,
    dac_i2c_send3_next,
    dac_i2c_stop,
    dac_i2c_stop_c1d0,
    dac_i2c_stop_c1d1,
    dac_idle
);
signal dac_init_stg : dacstg_t := dac_reset;
signal dac_scl_o : std_logic := '1';
signal dac_sda_o : std_logic := '1';
signal dac_scl_i : std_logic;
signal dac_sda_i : std_logic;

begin

dac_scl_i <= scl;                           -- I2C clock input
dac_sda_i <= sda;                           -- I2C data input (used to sense ACK by testing for dac_sda_i='0' when dac_sda_o='1')
with dac_scl_o select scl <=
    '0' when '0',                           -- pull down to send '0'
    'Z' when others;                        -- bus is pulled up so '1' is sent by breaking the connection (high-Z)
with dac_sda_o select sda <=
    '0' when '0',                           -- pull down to send '0'
    'Z' when others;                        -- bus is pulled up so '1' is sent by breaking the connection (high-Z)

init_line <= dac_init_line;

error <= not dac_ok;

i2c: process(clk1M,res0,dac_buffer,dac_x_offs,
             dac_init_line,dac_init_wait,dac_init_stg) is
variable cur_data : slv20;
variable nack : std_logic;
begin
    cur_data := init_data;
    nack := dac_sda_i;
    if (res0 = '0') then
        dac_init_stg <= dac_reset;
    elsif (rising_edge(clk1M)) then
        case dac_init_stg is
            when dac_reset =>
                dac_ok    <= '1';
                dac_scl_o <= '1';
                dac_sda_o <= '1';
                dac_init_line <= x"00";
                dac_init_wait <= x"0000";
                dac_init_stg <= dac_loadbuf;
            when dac_loadbuf =>
                dac_x_offs <= x"1a";                -- position in sequence
                dac_buffer <= dac_addr_w & '1' &    -- buffer the sequence (ACK bits are constant '1')
                    cur_data(18 downto 12) & cur_data(8) & '1' &
                    cur_data(7 downto 0) & '1';
                if (cur_data(19 downto 12)=x"09" and dac_init_wait<x"ffff") then
                    -- since the VMID delay is precondition to setting reg 09
                    -- the delay is performed when the current initialization
                    -- data indicate setting of reg 09
                    dac_init_wait <= dac_init_wait + 1;
                elsif (cur_data = x"fffff") then
                    -- init data of 0xfffff is end sentinel
                    dac_init_stg <= dac_idle;
                else
                    dac_init_stg <= dac_i2c_start;
                end if;                
            when dac_i2c_start =>
                dac_sda_o <= '0';
                dac_init_stg <= dac_i2c_send0_c0;
            when dac_i2c_send0_c0 =>
                dac_scl_o <= '0';
                dac_init_stg <= dac_i2c_send1_c0dx;
            when dac_i2c_send1_c0dx =>
                dac_sda_o <= dac_buffer(to_integer(dac_x_offs));
                dac_init_stg <= dac_i2c_send2_c1;
            when dac_i2c_send2_c1 =>
                dac_scl_o <= '1';
                dac_init_stg <= dac_i2c_send3_next;
            when dac_i2c_send3_next =>
                if ((dac_x_offs=x"00" or dac_x_offs=x"09" or dac_x_offs=x"12") and nack='1') then
                    dac_ok <= '0';
                    dac_init_stg <= dac_i2c_stop;
                elsif (dac_x_offs = x"00") then
                    dac_init_stg <= dac_i2c_stop;
                else
                    dac_x_offs <= dac_x_offs - 1;
                    dac_init_stg <= dac_i2c_send0_c0;
                end if;
            when dac_i2c_stop =>
                dac_scl_o <= '0';
                dac_sda_o <= '0';
                dac_init_stg <= dac_i2c_stop_c1d0;
            when dac_i2c_stop_c1d0 =>
                dac_scl_o <= '1';
                dac_init_stg <= dac_i2c_stop_c1d1;
            when dac_i2c_stop_c1d1 =>
                dac_sda_o <= '1';
                if (dac_ok = '0') then
                    dac_init_stg <= dac_idle;
                else
                    dac_init_line <= dac_init_line + 1;
                    dac_init_stg <= dac_loadbuf;
                end if;
            when others =>
                null;
        end case;
    end if; 
end process i2c;

end i2c_init_guts;
