--------------------------------------------------------------------------------
--  File:       div_ctrl.vhd
--  Desc:       Autogenerated block control module.
--
--  Author:     Isa Uzun - Diamond Light Source
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.addr_defines.all;
use work.top_defines.all;

entity div_ctrl is
port (
    -- Clock and Reset
    clk_i               : in std_logic;
    reset_i             : in std_logic;
    sysbus_i            : in sysbus_t;
    posbus_i            : in posbus_t;
    -- Block Parameters
    DIVISOR       : out std_logic_vector(31 downto 0);
    DIVISOR_WSTB  : out std_logic;
    FIRST_PULSE       : out std_logic_vector(31 downto 0);
    FIRST_PULSE_WSTB  : out std_logic;
    COUNT       : in  std_logic_vector(31 downto 0);
    inp_o : out std_logic;
    enable_o : out std_logic;
    -- Memory Bus Interface
    read_strobe_i       : in  std_logic;
    read_address_i      : in  std_logic_vector(BLK_AW-1 downto 0);
    read_data_o         : out std_logic_vector(31 downto 0);
    read_ack_o          : out std_logic;

    write_strobe_i      : in  std_logic;
    write_address_i     : in  std_logic_vector(BLK_AW-1 downto 0);
    write_data_i        : in  std_logic_vector(31 downto 0);
    write_ack_o         : out std_logic
);
end div_ctrl;

architecture rtl of div_ctrl is

signal read_addr        : natural range 0 to (2**read_address_i'length - 1);
signal write_addr       : natural range 0 to (2**write_address_i'length - 1);

signal INP      : std_logic_vector(31 downto 0);
signal INP_WSTB : std_logic;
signal INP_DLY      : std_logic_vector(31 downto 0);
signal INP_DLY_WSTB : std_logic;
signal ENABLE      : std_logic_vector(31 downto 0);
signal ENABLE_WSTB : std_logic;
signal ENABLE_DLY      : std_logic_vector(31 downto 0);
signal ENABLE_DLY_WSTB : std_logic;

begin

-- Unused outputs
read_ack_o <= '0';
write_ack_o <= '0';

read_addr <= to_integer(unsigned(read_address_i));
write_addr <= to_integer(unsigned(write_address_i));

--
-- Control System Interface
--
REG_WRITE : process(clk_i)
begin
    if rising_edge(clk_i) then
        if (reset_i = '1') then
            DIVISOR <= (others => '0');
            DIVISOR_WSTB <= '0';
            FIRST_PULSE <= (others => '0');
            FIRST_PULSE_WSTB <= '0';
            INP <= (others => '0');
            INP_WSTB <= '0';
            INP_DLY <= (others => '0');
            INP_DLY_WSTB <= '0';
            ENABLE <= (others => '0');
            ENABLE_WSTB <= '0';
            ENABLE_DLY <= (others => '0');
            ENABLE_DLY_WSTB <= '0';
        else
            DIVISOR_WSTB <= '0';
            FIRST_PULSE_WSTB <= '0';
            INP_WSTB <= '0';
            INP_DLY_WSTB <= '0';
            ENABLE_WSTB <= '0';
            ENABLE_DLY_WSTB <= '0';

            if (write_strobe_i = '1') then
                -- Input Select Control Registers
                if (write_addr = DIV_DIVISOR) then
                    DIVISOR <= write_data_i;
                    DIVISOR_WSTB <= '1';
                end if;
                if (write_addr = DIV_FIRST_PULSE) then
                    FIRST_PULSE <= write_data_i;
                    FIRST_PULSE_WSTB <= '1';
                end if;
                if (write_addr = DIV_INP) then
                    INP <= write_data_i;
                    INP_WSTB <= '1';
                end if;
                if (write_addr = DIV_INP_DLY) then
                    INP_DLY <= write_data_i;
                    INP_DLY_WSTB <= '1';
                end if;
                if (write_addr = DIV_ENABLE) then
                    ENABLE <= write_data_i;
                    ENABLE_WSTB <= '1';
                end if;
                if (write_addr = DIV_ENABLE_DLY) then
                    ENABLE_DLY <= write_data_i;
                    ENABLE_DLY_WSTB <= '1';
                end if;

            end if;
        end if;
    end if;
end process;

--
-- Status Register Read
--
REG_READ : process(clk_i)
begin
    if rising_edge(clk_i) then
        if (reset_i = '1') then
            read_data_o <= (others => '0');
        else
            case (read_addr) is
                when DIV_COUNT =>
                    read_data_o <= COUNT;
                when others =>
                    read_data_o <= (others => '0');
            end case;
        end if;
    end if;
end process;

--
-- Instantiate Delay Blocks for System and Position Bus Fields
--
bitmux_INP : entity work.bitmux
port map (
    clk_i       => clk_i,
    sysbus_i    => sysbus_i,
    bit_o       => inp_o,
    BITMUX_SEL  => INP,
    BIT_DLY     => INP_DLY
);

bitmux_ENABLE : entity work.bitmux
port map (
    clk_i       => clk_i,
    sysbus_i    => sysbus_i,
    bit_o       => enable_o,
    BITMUX_SEL  => ENABLE,
    BIT_DLY     => ENABLE_DLY
);




end rtl;