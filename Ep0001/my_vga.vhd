--
-- FPGA logic for simple VGA signal generator
--

library IEEE;
	use IEEE.std_logic_1164.all;
	use IEEE.numeric_std.all;

entity my_vga is
	port (
		clk_125   : in std_logic;
		vga_hs    : out std_logic;
		vga_vs    : out std_logic;
		vga_r     : out unsigned(4 downto 0);
		vga_g     : out unsigned(5 downto 0);
		vga_b     : out unsigned(4 downto 0)
	);
end my_vga;

architecture driver of my_vga is
	subtype tPeriod is unsigned(17 downto 0);
	constant pZero : tPeriod := to_unsigned(0,tPeriod'length);
	subtype hvCount is unsigned(15 downto 0);
	constant hvZero : hvCount := to_unsigned(0,hvCount'length);
	--
	-- VGA target is 800x600 @ 60 Hz
	-- www.tinyvga.com yields:
	-- pixel clock: 40 MHz (40000 kHz)
	--
	constant targetPeriod : tPeriod	:= to_unsigned(40000,tPeriod'length);
	constant refPeriod	: tPeriod	:= to_unsigned(62500,tPeriod'length);
	--
	-- frame timing:
	--   H(pixels) : +sync=128, front=40, visible=800, back=88 (total: 1056)
	--   V(lines)  : +sync=4,   front=1,  visible=600, back=23 (total: 628)
	--   NB: for 800x600 modes sync is active high hence why I write +sync
	--
	constant hSyncOn	: std_logic	:= '1';
	constant hSyncOff	: std_logic	:= '0';
	constant hSyncDur	: hvCount	:= to_unsigned(128,hvCount'length);
	constant hBackDur	: hvCount	:= to_unsigned(88,hvCount'length);
	constant hBegin		: hvCount	:= to_unsigned(128+88,hvCount'length);
	constant hCenterDur	: hvCount	:= to_unsigned(800,hvCount'length);
	constant hEnd		: hvCount	:= to_unsigned(128+88+800,hvCount'length);
	constant hFrontDur	: hvCount	:= to_unsigned(40,hvCount'length);
	constant hTotal		: hvCount	:= to_unsigned(1056,hvCount'length);
	constant vSyncOn	: std_logic := '1';
	constant vSyncOff	: std_logic := '0';
	constant vSyncDur	: hvCount	:= to_unsigned(4,hvCount'length);
	constant vBackDur	: hvCount	:= to_unsigned(23,hvCount'length);
	constant vBegin		: hvCount	:= to_unsigned(4+23,hvCount'length);
	constant vCenterDur	: hvCount	:= to_unsigned(600,hvCount'length);
	constant vEnd		: hvCount	:= to_unsigned(4+23+600,hvCount'length);
	constant vFrontDur	: hvCount	:= to_unsigned(1,hvCount'length);
	constant vTotal		: hvCount	:= to_unsigned(628,hvCount'length);

	signal pxClock : std_logic := '0';
	signal p40count : tPeriod := pZero;
	signal hPos : hvCount := hvZero;
	signal vPos : hvCount := hvZero;
begin
	--
	-- clock divider to generate pixel clock
	-- 125 Mhz (125000 kHz) input clock (Zybo 125 MHz ref clock L16)
	--
	-- We want to be 0 for half the period, then 1 for half the period.
	-- We want the total duty cycle to match the pixel clock rate.
	-- Thus we count in steps of 40000 to half the reference period
	-- (62500) and whenever we are >= 62500 we subtract 62500, then
	-- invert the pixel clock output signal.
	--
	-- This integer scaling is likely going to jitter like crazy however
	-- VGA is somewhat forgiving in this regard and it will realign on
    -- every LCM of the target and reference periods.  The only way to
	-- reduce the jitter is to reference from a faster clock (Zybo does
	-- not have anything faster than 125 MHz) or use one of the 7010's
	-- MMCM units.
	--
	pxClock_proc: process(clk_125) is
	variable p40var : tPeriod := p40count;
	begin
		if (rising_edge(clk_125)) then
			p40var := p40var + targetPeriod;
			if (p40var >= refPeriod) then
				p40var := p40var - refPeriod;
				pxClock <= not pxClock;
			end if;
			p40count <= p40var;
		end if;
	end process pxClock_proc;
	--
	-- Next we need the H and V counters, which will be advanced on the
	-- rising edges of the pixel clock
	--
	hv_proc: process(pxClock) is
	variable hCnt : hvCount := hPos;
	variable vCnt : hvCount := vPos;
	begin
		if (rising_edge(pxClock)) then
			hCnt := hCnt + 1;
			if (hCnt = hTotal) then
				hCnt := hvZero;
				vCnt := vCnt + 1;
				if (vCnt = vTotal) then
					vCnt := hvZero;
				end if;
				vPos <= vCnt;
			end if;
			hPos <= hCnt;
		end if;
	end process hv_proc;
	--
	-- Next we need to generate the sync pulses
	--
	-- Syncs pulses are first in the timing sequence thus
	-- making it easy to check for sync active range
	--
	-- H sync:
	hsync_proc: process(pxClock) is
	begin
	   if (rising_edge(pxClock)) then
           if (hPos < hSyncDur) then
               vga_hs <= hSyncOn;
           else
               vga_hs <= hSyncOff;
           end if;
		end if;
	end process hsync_proc;
	-- V sync:
	vsync_proc: process(pxClock) is
	begin
	   if (rising_edge(pxClock)) then
           if (vPos < vSyncDur) then
                vga_vs <= vSyncOn;
           else
               vga_vs <= vSyncOff;
           end if;
	   end if;
	end process vsync_proc;
	--
	-- With H and V sync pulses taken care of means we could now generate pixel
	-- data at the appropriate time, meaning when we are in both the horizontal
	-- and vertical visible range.
	--
	pattern_proc: process(pxClock) is
	variable pixH : hvCount := hvZero;
	variable pixV : hvCount := hvZero;
	variable tempR : unsigned(31 downto 0) := to_unsigned(0,32);
	variable tempG : unsigned(31 downto 0) := to_unsigned(0,32);
	variable tempB : unsigned(31 downto 0) := to_unsigned(0,32);
	begin
	    if (rising_edge(pxClock)) then
            if  ((vPos >= vBegin) and (vPos < vEnd)) and ((hPos >= hBegin) and (hPos < hEnd)) then
                pixV := vPos - vBegin;
                pixH := hPos - hBegin;
                -- generates a color gradation pattern
                --tempR:=to_unsigned(0,32);
                tempR:=(pixH*32)/hCenterDur;
                tempG:=to_unsigned(0,32);
                --tempG:=pixH*1;
                --tempG:=(pixH*64)/hCenterDur;
                --tempB:=to_unsigned(0,32);
                --tempB:=pixV*1;
                tempB:=(pixV*32)/vCenterDur;
                vga_r <= tempR(4 downto 0);
                vga_g <= tempG(5 downto 0);
                vga_b <= tempB(4 downto 0);
            else
                -- Ensure pixels are zero outside of visible range.
                -- We might get away with the porches (for a border color),
                -- but NEVER have these nonzero during the sync pulses!
                vga_r <= to_unsigned(0,vga_r'length);
                vga_g <= to_unsigned(0,vga_g'length);
                vga_b <= to_unsigned(0,vga_b'length);
            end if;
		end if;
	end process pattern_proc;
end driver;

