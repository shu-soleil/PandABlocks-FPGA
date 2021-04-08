--==============================================================================
-- Company        : Synchrotron SOLEIL
-- Project        : PandABox FPGA
-- Design name    : sfp_udpontrig
-- Module name    : udp_ping.vhd
-- Purpose        : ICMP layer which responds only to echo requests with an echo reply
--                  Any other ICMP messages are discarded (ignored).
--                  Can respond to any ping containing up to 1472 bytes of data.
--                  ie 1500 bytes - 20 (IP Header) - 8 (ICMP header)
--
-- Author         : Thierry GARREL (ELSYS-Design)
-- Synthesizable  : YES
-- Language       : VHDL-93
--------------------------------------------------------------------------------
-- Copyright (c) 2021 Synchrotron SOLEIL - L'Orme des Merisiers Saint-Aubin
-- BP 48 91192 Gif-sur-Yvette Cedex  - https://www.synchrotron-soleil.fr
--------------------------------------------------------------------------------

-- TODO
-- 2 clocks inputs : rx_clk (RX part) and tx_clk (TX part)



--==============================================================================
-- Libraries Declaration
--==============================================================================
library IEEE;
  use IEEE.std_logic_1164.all;
  use IEEE.numeric_std.all;

library work;
  use work.axi_types.all;
  use work.ipv4_types.all;
  use work.icmp_types.all;


-- =============================================================================
-- Entity Declaration
--==============================================================================
entity udp_ping is
  port (
    -- System signals (in)
    clk                   : in  std_logic;                      -- asynchronous clock
    reset                 : in  std_logic;                      -- synchronous active high reset input
    -- IP layer RX signals (in)
    ip_rx_start           : in  std_logic;                      -- indicates receipt of ip frame
    ip_rx                 : in  ipv4_rx_type;                   -- IP rx cxns
    -- status signals
    icmp_pkt_count        : out std_logic_vector(7 downto 0);   -- number of ICMP pkts received for us
    -- IP layer TX signals (out)
    ip_tx_start           : out  std_logic;
    ip_tx                 : out ipv4_tx_type;                   -- IP tx cxns
    ip_tx_result          : in  std_logic_vector(1 downto 0);   -- tx status (changes during transmission)
    ip_tx_data_out_ready  : in  std_logic                       -- indicates IP TX is ready to take data
    );
end udp_ping;

-- ip_rx (ipv4_rx_type)
-- .hdr   : .is_valid .protocol(7:0) .data_length(15:0) .src_ip_addr(31:0) .num_frame_errors(7:0) .last_error_code(3:0) .is_broadcast
-- .data  : .data_in(7:0) .data_in_valid .data_in_last


-- ip_tx (ipv4_tx_type)
-- .hdr    : .protocol(7:0) .data_length(15:0) .dst_ip_addr(31:0)
-- .data   : .data_out(7:0) .data_out_valid .data_out_last

-- ip_tx_result
-- IPTX_RESULT_NONE     : std_logic_vector(1 downto 0) := "00";
-- IPTX_RESULT_SENDING  : std_logic_vector(1 downto 0) := "01";
-- IPTX_RESULT_ERR      : std_logic_vector(1 downto 0) := "10";
-- IPTX_RESULT_SENT     : std_logic_vector(1 downto 0) := "11";


-------------------------------------------------------
-- description:  PING protocol.
-------------------------------------------------------
-- Reads receive packet structure on the fly and generates a ping echo.
-- Any new received packet is presumed to be an ICMP echo (ping) request.
-- Within a few bytes, information is received as to the real protocol associated with the received packet.
-- The ping echo generation is immediately cancelled if :
-- (a) the received packet type is not an IP datagram
-- (b) the received IP type is not ICMP (RX_IP_TYPE /= 1)
-- (c) invalid destination IP
-- (d) ICMP incoming packet is not an echo request (ICMP type /= x"0800")
-- (e) packet size is greater than MAX_PING_SIZE bytes
--
-- -----------------------------------------------------------------------


-- ICMP : RF972 https://tools.ietf.org/html/rfc792
--
-- Couche 3 => Encapsulé dans un datagramme IP
-- Entêete IP : Protocole = 1 (ICMP) Type de Service = 0
-- voir https://fr.wikipedia.org/wiki/Internet_Control_Message_Protocol
-- et https://www.iana.org/assignments/icmp-parameters/icmp-parameters.xhtml#icmp-parameters-codes-0


-- Paquet ICMP encapsule dans un datagrame IP : https://en.wikipedia.org/wiki/IPv4
-- ================================================================================
-- IP datagram header format ( https://www.frameip.com/entete-ip ; https://rlworkman.net/howtos/iptables/chunkyhtml/x178.html )
--
--
--    0          4          8                      16      19             24                    31     bytes
--    --------------------------------------------------------------------------------------------
--    | Version  |  IHL     |    Service Type      |        Total Length including header        |     4   ^
--    --------------------------------------------------------------------------------------------         |
--    |           Identification                   | Flags |  Fragment Offset (in 32 bit words)  |     4   |
--    --------------------------------------------------------------------------------------------         |
--    |    TTL (ignored)    |     Protocol 0x01    |             Header Checksum                 |     4   | 20 bytes (160 bits) min
--    --------------------------------------------------------------------------------------------         |
--    |                                   Source IP Address                                      |     4   |
--    --------------------------------------------------------------------------------------------         |
--    |                                 Destination IP Address                                   |     4   v
--    --------------------------------------------------------------------------------------------
--    |                                   Options (if IHL > 5)                                   |     0 - 40 bytes
--    --------------------------------------------------------------------------------------------
-- Version (4 bits)       : "0100" – IP V4
-- IHL     (4 bits)       : Internet header length (in 32 bits words) default = "0101" (5) => 20 bytes
-- Version/IHL            = 0x"45"
-- Total Length (16 bits) : packet length including header and data (in bytes)
          --                Data Length = Total Length – ( IHL * 4 )
-- Protocol (8 bits)      :  1 – 0x01 – ICMP
--                          17 – 0x11 – UDP
--
--
-- ICMP data protocol unit header format ( https://www.frameip.com/entete-icmp/ )
-- Standard echo request and reply ICMP header : 8 bytes minimum
--
--    0                      8                     16                                           31     bytes
--    --------------------------------------------------------------------------------------------
--    |         Type         |        Code         |                Checksum                     | (1)  4   |
--    --------------------------------------------------------------------------------------------          | 8 bytes min
--    |                Identifier                  |              Sequence_number                |      4   |
--    --------------------------------------------------------------------------------------------
--    |                                      Optional Data                                       |
--    --------------------------------------------------------------------------------------------
--    |                                          ....                                            |
--    --------------------------------------------------------------------------------------------
--
-- Type = Type de message (8 bits)            PING : Echo Request  : Type 8 Code 0
-- Code = Code de l’erreur (8 bits)                  Echo Reply    : Type 0 Code 0
-- Checksum (16 bits) calculée sur la partie spécifique à l’ICMP (sans l’entête IP)
-- (1) used by all of the ICMP types ( https://rlworkman.net/howtos/iptables/chunkyhtml/x281.html )
--
-- Identifiant et Numéro de Séquence ou Bourrage (32 bits)
--
-- Les données reçues dans un message d’écho doivent être réémises dans la réponse.
-- Ainsi, si le message de retour correspond à l’émission, on en déduit que l’Hote est présent.




--==============================================================================
-- Entity Architecture
--==============================================================================
architecture behavioral of udp_ping is

  -- RX side :  receive IP datagram with protocol = ICMP (0x01) :
--              -detects Echo Request (Type 08 Code 00)
  --            -prepare Echo Reply response

  -- TX side :  send IP datagram with protocol = ICMP (0x01)
  --            dest_ip_addr = src_ip_addr
  --            and Echo Reply response  (Type 00 code 00)
  --

  -------------------------------------------------
  -- Common type definitions
  -------------------------------------------------
  type count_mode_type            is (RST, INCR, HOLD);
  type settable_count_mode_type   is (RST, SET_VAL, INCR, HOLD);
  type set_clr_type               is (SET, CLR, HOLD);
  type rx_event_type              is (NO_EVENT, DATA);

  type rx_state_type              is (IDLE, ICMP_HEADER, USER_DATA, WAIT_END, ERR);
  type tx_state_type              is (IDLE, PAUSE, SEND_ICMP_HEADER, SEND_USER_DATA);

  --------------------------------
  -- inputs followers signals
  --------------------------------
  signal rx_event           : rx_event_type; -- DATA, NO_EVENT
  signal rx_data_length     : unsigned(15 downto 0);  -- data_length extracted from ip_rx header  ip_rx.hdr.data_length

  --------------------------------
  -- inputs register pipeline
  --------------------------------
  type data_in_array is array(1 to 3) of axi_in_type; -- data_in data_in_valid data_in_last
  signal rx_data_reg      : data_in_array;

  -------------------------------------------------
  -- RX side states and signals definition
  -------------------------------------------------
  -- rx state variables
  signal set_rx_state       : std_logic;      -- go to next state else stay in current state
  signal next_rx_state      : rx_state_type;  -- next state register
  signal rx_state           : rx_state_type;  -- state register
  -- rx control signals
  signal rx_count_mode      : settable_count_mode_type;
  signal rx_count           : unsigned(15 downto 0);

  signal set_pkt_cnt        : count_mode_type;
  signal rx_pkt_counter     : unsigned(7 downto 0);  -- number of ICMP pkts received for us

  -- capture ICMP header fields
  signal set_src_ip         : std_logic;
  signal set_identifier_H   : std_logic;
  signal set_identifier_L   : std_logic;
  signal set_seq_number_H   : std_logic;
  signal set_seq_number_L   : std_logic;

  signal icmp_rx_header     : icmp_header_type; -- src_ip_addr  data_length  msg_type msg_code checksum identifier  seq_number

  signal set_echo_request   : std_logic;      -- indicate we have receive an ICMP Echo Request rame (1 clk pulse)
  signal icmp_echo_request  : std_logic;
  signal icmp_echo_reply    : std_logic;      -- indicates to create ICMP Echo reply response



  -------------------------------------------------
  -- TX side states and signals definition
  -------------------------------------------------
  -- tx state variables
  signal set_tx_state       : std_logic;
  signal next_tx_state      : tx_state_type;
  signal tx_state           : tx_state_type;  -- TX current state register
    -- tx control signals
  signal tx_count_mode      : settable_count_mode_type;
  signal tx_count           : unsigned(15 downto 0);

  signal set_ip_tx_start    : set_clr_type;
  signal ip_tx_start_reg    : std_logic;          -- precurseur ip_tx_start (out)

  signal tx_data            : axi_out_type;       -- precurseru ip_tx_data (out)

  --------------------------------
  -- outputs followers signals
  --------------------------------
  signal ip_tx_header       : ipv4_tx_header_type; -- .protocol(7:0) .data_length(15:0) .dst_ip_addr(31:0)
  signal ip_tx_data         : axi_out_type;


  --------------------------------
  -- Signal attributes (debug)
  --------------------------------
  attribute fsm_encoding    : string;
  attribute fsm_safe_state  : string;
  attribute mark_debug      : string;

  attribute fsm_encoding   of rx_state : signal is "one_hot";
  attribute fsm_safe_state of rx_state : signal is "auto_safe_state";
  attribute mark_debug     of rx_state : signal is "true";

  attribute fsm_encoding   of tx_state : signal is "one_hot";
  attribute fsm_safe_state of tx_state : signal is "auto_safe_state";
  attribute mark_debug     of tx_state : signal is "true";


--==============================================================================
-- Beginning of Code
--==============================================================================
begin

  -- inputs followers

  -- determine event (if any)   // dataval (udp_rx, ipv4_rx)
  rx_event        <= DATA    when (ip_rx.data.data_in_valid = '1') else NO_EVENT;
  rx_data_length  <= unsigned(ip_rx.hdr.data_length) - 8 ;  -- user_data length = total length -8 (header length)


  -- ***************************************************************************
  --                             ICMP Rx part
  -- ***************************************************************************

  -----------------------------------------------------------------------
  -- RX combinatorial process to implement FSM and determine control signals
  -----------------------------------------------------------------------
  rx_combinatorial : process (
    -- input signals
    ip_rx_start, ip_rx,
    -- state variables
    rx_state, rx_count,
    -- control signals
    next_rx_state, set_rx_state, set_pkt_cnt, rx_event, rx_count_mode,
    set_src_ip, set_identifier_H, set_identifier_L, set_seq_number_H, set_seq_number_L
    )
  begin

    -- set signal defaults
    rx_count_mode     <= HOLD;
    next_rx_state     <= IDLE;
    set_rx_state      <= '0';  -- HOLD rx_state
    set_pkt_cnt       <= HOLD;

    set_src_ip        <= '0';
    set_identifier_H  <= '0';
    set_identifier_L  <= '0';
    set_seq_number_H  <= '0';
    set_seq_number_L  <= '0';
    set_echo_request  <= '0';

    -- RX_FSM combinatorial
    case rx_state is
      -----------------
      -- IDLE
      -----------------
      when IDLE =>
        rx_count_mode <= RST;
        case rx_event is
          when NO_EVENT =>                          -- (nothing to do)
          when DATA =>
            if ip_rx.hdr.protocol = ICMP_PROTOCOL then      -- ICMP protocol x"01"

              -- ignore pkts that are not ICMP type 08 (Echo Request)
              if (ip_rx.data.data_in /= ICMP_TYPE_08) then
                rx_count_mode   <= RST;
                next_rx_state   <= WAIT_END;
                set_rx_state    <= '1';
              else
                rx_count_mode   <= INCR;
                next_rx_state   <= ICMP_HEADER;
                set_rx_state    <= '1';
              end if;

            else                                    -- non ICMP protocol - ignore this pkt

              next_rx_state   <= WAIT_END;
              set_rx_state    <= '1';
            end if;
        end case;

      -----------------
      -- ICMP_HEADER
      -----------------
      when ICMP_HEADER =>
        case rx_event is
          when NO_EVENT =>              -- (nothing to do)
          when DATA =>
            if rx_count = to_unsigned(ICMP_HEADER_LENGTH-1,16) then       -- 7
              rx_count_mode   <= SET_VAL; -- rx_count restarts at 1
              next_rx_state   <= USER_DATA;
              set_rx_state    <= '1';
              set_pkt_cnt     <= INCR; -- count another pkt received
            else
              rx_count_mode   <= INCR;
              next_rx_state   <= ICMP_HEADER;
            end if;

            -- handle early frame termination
            if ip_rx.data.data_in_last = '1' then
              next_rx_state   <= IDLE;
              set_rx_state    <= '1';
            else
              case rx_count is
                -- ICMP header
                when x"0001" => -- ignore pkts that are not ICMP Code 00 (Echo Request)
                                if (ip_rx.data.data_in /= ICMP_CODE_00) then
                                  rx_count_mode   <= RST;
                                  next_rx_state   <= WAIT_END;
                                  set_rx_state    <= '1';
                                else
                                  set_src_ip <= '1';  -- capture ip_rx header
                                end if;

                when x"0002" => -- ignore checksum
                when x"0003" => -- ignore checksum

                when x"0004" => set_identifier_H <= '1';    -- capture ICMP header Identificier field msb
                when x"0005" => set_identifier_L <= '1';    -- capture ICMP header Identificier field lsb
                when x"0006" => set_seq_number_H <= '1';    -- capture ICMP header Sequence_number field msb
                when x"0007" => set_seq_number_L <= '1';    -- capture ICMP header Sequence_number field lsb

                                -- we have an ICMP Echo Request (Type 08)
                                set_echo_request <= '1';


                when others =>  -- ignore other bytes in ICMP header
              end case;
            end if;
        end case;

      when USER_DATA =>
        case rx_event is
          when NO_EVENT =>              -- (nothing to do)
              -- check for early frame termination  ///// TGA
              -- TODO need to mark frame as errored
              next_rx_state   <= IDLE;
              set_rx_state    <= '1';

          when DATA =>                  -- note: data gets transfered upstream as part of "outputs followers" processing

            if rx_count = (rx_data_length) then     -- end of ip_rx frame
              rx_count_mode <= RST;

              if ip_rx.data.data_in_last = '1' then
                next_rx_state <= IDLE;
                set_rx_state  <= '1';
              else
                next_rx_state <= WAIT_END;
                set_rx_state  <= '1';
              end if;

            else
              rx_count_mode <= INCR;
              -- check for early frame termination
              -- TODO need to mark frame as errored
              if ip_rx.data.data_in_last = '1' then
                next_rx_state <= IDLE;
                set_rx_state  <= '1';
              end if;
            end if;
        end case;


      when ERR =>
        if ip_rx.data.data_in_last = '0' then
          next_rx_state <= WAIT_END;
          set_rx_state    <= '1';
        else
          next_rx_state <= IDLE;
          set_rx_state    <= '1';
        end if;


      when WAIT_END =>
        case rx_event is
          when NO_EVENT =>              -- (nothing to do)
          when DATA =>
            if ip_rx.data.data_in_last = '1' then
              next_rx_state   <= IDLE;
              set_rx_state    <= '1';
            end if;
        end case; -- rx_event

    end case; -- rx_state;
  end process rx_combinatorial;


  -----------------------------------------------------------------------------------
  -- RX sequential process to action control signals and change states and outputs
  -----------------------------------------------------------------------------------
  rx_sequential : process(clk)
  begin
    if rising_edge(clk) then
      if reset = '1' then
        -- reset state variables
        rx_state            <= IDLE;
        rx_count            <= x"0000";
        rx_pkt_counter      <= x"00";
        ip_tx_start_reg     <= '0';
        icmp_echo_request   <= '0';

        icmp_rx_header      <= ICMP_HEADER_ZERO;

      else
        icmp_echo_request <= set_echo_request;
        -- Next rx_state processing
        if set_rx_state = '1' then
          rx_state <= next_rx_state;
        else
          rx_state <= rx_state;
        end if;
        -- rx_count processing
        case rx_count_mode is
          when RST     => rx_count <= x"0000";      -- reset
          when SET_VAL => rx_count <= x"0001";      -- set
          when INCR    => rx_count <= rx_count + 1; -- increment
          when HOLD    => rx_count <= rx_count;     -- no change
        end case;

        -- ip_tx_start_reg processing
        case set_ip_tx_start is
          when SET  => ip_tx_start_reg <= '1';
          when CLR  => ip_tx_start_reg <= '0';
          when HOLD => ip_tx_start_reg <= ip_tx_start_reg;
        end case;

        -- icmp pkt processing
        case set_pkt_cnt is
          when RST  => rx_pkt_counter <= x"00";
          when INCR => rx_pkt_counter <= rx_pkt_counter + 1;
          when HOLD => rx_pkt_counter <= rx_pkt_counter;
        end case;


        -----------------------------------------------------------------
        -- Populate icmp_rx header to prepare Echo Reply response
        -----------------------------------------------------------------
        if (set_src_ip = '1') then
          icmp_rx_header.src_ip_addr  <= ip_rx.hdr.src_ip_addr; -- capture src_IP address from ip_rx header
          icmp_rx_header.data_length  <= ip_rx.hdr.data_length; -- capture data_length    from ip_rx header
          icmp_rx_header.msg_type     <= ICMP_TYPE_00;          -- Echo Reply Type 00 Code 00
          icmp_rx_header.msg_code     <= ICMP_CODE_00;
          icmp_rx_header.checksum     <= x"AA55";               -- checksum will be computed by ipv4_tx module (use only for TEST)
        end if;

        -- capture ICMP header fields :
        if (set_identifier_H = '1') then
          icmp_rx_header.identifier(15 downto 8)  <= ip_rx.data.data_in;
        end if;
        if (set_identifier_L = '1') then
          icmp_rx_header.identifier(7 downto 0)   <= ip_rx.data.data_in;
        end if;
        if (set_seq_number_H = '1') then
          icmp_rx_header.seq_number(15 downto 8)  <= ip_rx.data.data_in;
        end if;
        if (set_seq_number_L = '1') then
          icmp_rx_header.seq_number(7 downto 0)   <= ip_rx.data.data_in;
        end if;


      end if; -- reset
    end if; -- clk
  end process rx_sequential;


  -- ***************************************************************************
  --                          ICMP Tx part
  -- ***************************************************************************
  icmp_echo_reply <= icmp_echo_request;



  tx_combinatorial : process (
    -- input signals
    icmp_echo_reply, ip_tx_result, ip_tx_data_out_ready,
    -- state variables
    tx_state, tx_count, ip_tx_start_reg,
    -- control signals
    next_tx_state, set_tx_state, tx_count_mode, tx_data,
    rx_data_length, set_ip_tx_start
    )
    begin

      -- set signal defaults
      next_tx_state       <= IDLE;
      set_tx_state        <= '0';
      tx_count_mode       <= HOLD;
      set_ip_tx_start     <= HOLD;

      --tx_data.data_out        <= x"00";
      tx_data.data_out_valid  <= '0';
      tx_data.data_out_last   <= '0';

      -- TX_FSM combinatorial
      case tx_state is
      -----------------
      -- IDLE
      -----------------
      when IDLE =>

        tx_count_mode <= RST;

        -- wait until we have received en ICMP Echo Request (ping)
        if icmp_echo_reply = '1' then
            -- start to send UDP header
            tx_count_mode   <= RST;
            next_tx_state   <= PAUSE;
            set_tx_state    <= '1';
            set_ip_tx_start <= SET;
        end if;

      -----------------
      -- PAUSE
      -----------------
      when PAUSE =>
        -- delay one clock for IP layer to respond to ip_tx_start and remove any tx error result

        if ip_tx_data_out_ready = '1' then        -- TODO check validity

          next_tx_state <= SEND_ICMP_HEADER;
          set_tx_state <= '1';
        end if;

      ---------------------
      -- SEND ICMP HEADER
      ---------------------
      when SEND_ICMP_HEADER =>

        if ip_tx_result = IPTX_RESULT_ERR then        -- 0x10
          ---+++tx_data.data_out_valid <= '0';
          set_ip_tx_start <= CLR;
          next_tx_state   <= IDLE;
          set_tx_state    <= '1';
        else
          -- wait until ip tx is ready to accept data
          if ip_tx_data_out_ready = '1' then
            tx_data.data_out_valid <= '1';

            if tx_count = to_unsigned(ICMP_HEADER_LENGTH-1,16) then       -- 7
              tx_count_mode <= RST;    --   /// SET_VAL
              next_tx_state <= SEND_USER_DATA;
              set_tx_state  <= '1';
            else
              tx_count_mode <= INCR;
              next_tx_state <= SEND_ICMP_HEADER;
            end if;

            case tx_count is
              when x"0000" => tx_data.data_out <= ICMP_TYPE_00; -- Echo Reply Type 00 Code 00
              when x"0001" => tx_data.data_out <= ICMP_CODE_00;
              when x"0002" => tx_data.data_out <= icmp_rx_header.checksum(15 downto 8);
              when x"0003" => tx_data.data_out <= icmp_rx_header.checksum( 7 downto 0);
              when x"0004" => tx_data.data_out <= icmp_rx_header.identifier(15 downto 8);
              when x"0005" => tx_data.data_out <= icmp_rx_header.identifier( 7 downto 0);
              when x"0006" => tx_data.data_out <= icmp_rx_header.seq_number(15 downto 8);
              when x"0007" => tx_data.data_out <= icmp_rx_header.seq_number( 7 downto 0);
              when others =>
                -- shouldnt get here - handle as error
            end case;
          else
            -- IP Tx not ready to accept data
            ---+++tx_data.data_out_valid <= '0';
            next_tx_state <= SEND_ICMP_HEADER;
            tx_count_mode <= HOLD;
          end if;
        end if; -- ip_tx_result /= IPTX_RESULT_ERR and ip_tx_data_out_ready = '1'

      ---------------------
      -- SEND USER DATA
      ---------------------
      when SEND_USER_DATA =>      -- Send dummy data (not the same as Echo Request input frame)

        -- wait until ip tx is ready to accept data
        if ip_tx_data_out_ready = '1' then
          tx_data.data_out_valid <= '1';
          tx_data.data_out       <= std_logic_vector(tx_count(7 downto 0));  -- Send dummy data /// TODO replace by 0 ??

          if tx_count = unsigned(rx_data_length)-1 then
            -- TX terminated due to count - end normally
            tx_count_mode   <= RST;
            next_tx_state   <= IDLE;
            set_tx_state    <= '1';
            tx_data.data_out_last    <= '1';
            set_ip_tx_start <= CLR;
          else
            -- TX continues
            tx_count_mode   <= INCR;
            next_tx_state   <= SEND_USER_DATA;
          end if;

        else
          -- IP Tx not ready to accept data
          ---++++tx_data.data_out_valid <= '0';
          next_tx_state <= SEND_USER_DATA;
          tx_count_mode <= HOLD;
        end if;

      end case; -- tx_state
  end process tx_combinatorial;


  -----------------------------------------------------------------------------------
  -- TX sequential process to action control signals and change states and outputs
  -----------------------------------------------------------------------------------
  tx_sequential : process(clk)
  begin
    if rising_edge(clk) then
      if reset = '1' then
        -- reset state variables
        tx_state        <= IDLE;
        tx_count        <= (others=>'0');
        ip_tx_start_reg <= '0';
        ---
        ip_tx_header.protocol      <= ICMP_PROTOCOL; -- 0x01
        ip_tx_header.data_length   <= (others=>'0');
        ip_tx_header.dst_ip_addr   <= (others=>'0');
        --
        ip_tx_data.data_out       <= (others=>'0');
        ip_tx_data.data_out_valid <= '0';
        ip_tx_data.data_out_last  <= '0';
      else
        -- Next tx_state processing
        if set_tx_state = '1' then
          tx_state <= next_tx_state;
        else
          tx_state <= tx_state;
        end if;
        -- tx_count processing
        case tx_count_mode is
          when RST     => tx_count <= x"0000";      -- reset
          when SET_VAL => tx_count <= x"0001";      -- set_value
          when INCR    => tx_count <= tx_count + 1; -- increment
          when HOLD    => tx_count <= tx_count;     -- no change
        end case;
        -- ip_tx_start_reg processing
        case set_ip_tx_start is
          when SET  => ip_tx_start_reg <= '1';
          when CLR  => ip_tx_start_reg <= '0';
          when HOLD => ip_tx_start_reg <= ip_tx_start_reg; -- no change
        end case;
        -- ip_tx header
        if icmp_echo_reply = '1' then
          ip_tx_header.protocol      <= ICMP_PROTOCOL;
          ip_tx_header.data_length   <= icmp_rx_header.data_length;
          ip_tx_header.dst_ip_addr   <= icmp_rx_header.src_ip_addr;
        end if;
        -- ip_tx_data output
        ip_tx_data.data_out       <= tx_data.data_out;
        ip_tx_data.data_out_valid <= tx_data.data_out_valid ; --- and ip_tx_data_out_ready;
        ip_tx_data.data_out_last  <= tx_data.data_out_last;

      end if; -- reset
    end if; -- clk
  end process tx_sequential;

  --ip_tx_s.data.data_out_valid <= tx_data.data_out_valid and ip_tx_data_out_ready;


  ----------------------------------------
  -- outputs followers assignements
  --------------------------------------
  ip_tx_start     <= ip_tx_start_reg;
  ip_tx.hdr       <= ip_tx_header;
  ip_tx.data      <= ip_tx_data;

  icmp_pkt_count  <= std_logic_vector(rx_pkt_counter);



end behavioral;
--==============================================================================
-- End of Code
--==============================================================================

