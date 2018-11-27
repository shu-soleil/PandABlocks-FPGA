-- AUTOGENERATED
--------------------------------------------------------------------------------
-- Top-level VHDL wrapper for a block
-- This is responsible for creating 8 instances of a LUT Block
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_misc.all;

library work;
use work.top_defines.all;


entity lut_wrapper is
generic (
    NUM : natural := 8
);
port (
    -- Clocks and Resets
    clk_i               : in  std_logic := '0';
    reset_i             : in  std_logic := '0';

    -- Bus inputs
    -- TODO: rename to bit_bus_i
    bit_bus_i           : in  sysbus_t := (others => '0');
    pos_bus_i           : in  posbus_t := (others => (others => '0'));

    -- Bus outputs
    OUT_o               : out std_logic_vector(NUM-1 downto 0);

    -- Memory Interface
    read_strobe_i       : in  std_logic := '0';
    read_address_i      : in  std_logic_vector(PAGE_AW-1 downto 0) := (others => '0');
    read_data_o         : out std_logic_vector(31 downto 0);
    read_ack_o          : out std_logic;

    write_strobe_i      : in  std_logic := '0';
    write_address_i     : in  std_logic_vector(PAGE_AW-1 downto 0) := (others => '0');
    write_data_i        : in  std_logic_vector(31 downto 0) := (others => '0');
    write_ack_o         : out std_logic
);
end lut_wrapper;

architecture rtl of lut_wrapper is


-- Register addresses, current values and strobes, an array of these for NUM
-- Blocks

signal INPA        : std32_array(NUM-1 downto 0);
signal INPA_wstb   : std_logic_vector(NUM-1 downto 0);

signal INPA_dly        : std32_array(NUM-1 downto 0);
signal INPA_dly_wstb   : std_logic_vector(NUM-1 downto 0);

signal INPB        : std32_array(NUM-1 downto 0);
signal INPB_wstb   : std_logic_vector(NUM-1 downto 0);

signal INPB_dly        : std32_array(NUM-1 downto 0);
signal INPB_dly_wstb   : std_logic_vector(NUM-1 downto 0);

signal INPC        : std32_array(NUM-1 downto 0);
signal INPC_wstb   : std_logic_vector(NUM-1 downto 0);

signal INPC_dly        : std32_array(NUM-1 downto 0);
signal INPC_dly_wstb   : std_logic_vector(NUM-1 downto 0);

signal INPD        : std32_array(NUM-1 downto 0);
signal INPD_wstb   : std_logic_vector(NUM-1 downto 0);

signal INPD_dly        : std32_array(NUM-1 downto 0);
signal INPD_dly_wstb   : std_logic_vector(NUM-1 downto 0);

signal INPE        : std32_array(NUM-1 downto 0);
signal INPE_wstb   : std_logic_vector(NUM-1 downto 0);

signal INPE_dly        : std32_array(NUM-1 downto 0);
signal INPE_dly_wstb   : std_logic_vector(NUM-1 downto 0);

signal A        : std32_array(NUM-1 downto 0);
signal A_wstb   : std_logic_vector(NUM-1 downto 0);

signal B        : std32_array(NUM-1 downto 0);
signal B_wstb   : std_logic_vector(NUM-1 downto 0);

signal C        : std32_array(NUM-1 downto 0);
signal C_wstb   : std_logic_vector(NUM-1 downto 0);

signal D        : std32_array(NUM-1 downto 0);
signal D_wstb   : std_logic_vector(NUM-1 downto 0);

signal E        : std32_array(NUM-1 downto 0);
signal E_wstb   : std_logic_vector(NUM-1 downto 0);

signal FUNC        : std32_array(NUM-1 downto 0);
signal FUNC_wstb   : std_logic_vector(NUM-1 downto 0);


-- Register interface common

signal read_strobe      : std_logic_vector(NUM-1 downto 0);
signal read_data        : std32_array(NUM-1 downto 0);
signal write_strobe     : std_logic_vector(NUM-1 downto 0);
signal read_addr        : natural range 0 to (2**read_address_i'length - 1);
signal write_addr       : natural range 0 to (2**write_address_i'length - 1);
signal read_ack         : std_logic_vector(NUM-1 downto 0);

begin

    -- Acknowledgement to AXI Lite interface
    write_ack_o <= '1';
    read_ack_o <= or_reduce(read_ack);
    read_data_o <= read_data(to_integer(unsigned(read_address_i(PAGE_AW-1 downto BLK_AW))));

    -- Generate NUM instances of the blocks
    GEN : FOR I IN 0 TO (NUM-1) GENERATE

        -- Sub-module address decoding
        read_strobe(I) <= compute_block_strobe(read_address_i, I) and read_strobe_i;
        write_strobe(I) <= compute_block_strobe(write_address_i, I) and write_strobe_i;


        lut_ctrl : entity work.lut_ctrl
        port map (
            clk_i               => clk_i,
            reset_i             => reset_i,
            bit_bus_i           => bit_bus_i,
            pos_bus_i           => pos_bus_i,

            INPA_from_bus       => INPA(I)(0),
            INPB_from_bus       => INPB(I)(0),
            INPC_from_bus       => INPC(I)(0),
            INPD_from_bus       => INPD(I)(0),
            INPE_from_bus       => INPE(I)(0),
            A                   => A(I),
            A_wstb              => A_wstb(I),
            B                   => B(I),
            B_wstb              => B_wstb(I),
            C                   => C(I),
            C_wstb              => C_wstb(I),
            D                   => D(I),
            D_wstb              => D_wstb(I),
            E                   => E(I),
            E_wstb              => E_wstb(I),
            FUNC                => FUNC(I),
            FUNC_wstb           => FUNC_wstb(I),

            read_strobe_i       => read_strobe(I),
            read_address_i      => read_address_i(BLK_AW-1 downto 0),
            read_data_o         => read_data(I),
            read_ack_o          => read_ack(I),

            write_strobe_i      => write_strobe(I),
            write_address_i     => write_address_i(BLK_AW-1 downto 0),
            write_data_i        => write_data_i,
            write_ack_o         => open
        );

        -- Connect to the actual logic entity
        lut : entity work.lut
        port map (
            INPA_i              => INPA(I)(0),
            INPB_i              => INPB(I)(0),
            INPC_i              => INPC(I)(0),
            INPD_i              => INPD(I)(0),
            INPE_i              => INPE(I)(0),
            FUNC                => FUNC(I),
            A                   => A(I)(1 downto 0),
            B                   => B(I)(1 downto 0),
            C                   => C(I)(1 downto 0),
            D                   => D(I)(1 downto 0),
            E                   => E(I)(1 downto 0),
            OUT_o               => OUT_o(I),
            clk_i               => clk_i
        );

    END GENERATE;

end rtl;
