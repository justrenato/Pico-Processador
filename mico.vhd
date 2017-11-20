-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- UFPR, BCC, ci210 2017-2 trabalho semestral, autor: Roberto Hexsel, 21out
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- processador MICO XI
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee; use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.p_wires.all;


entity mico is
port (rst,clk : in bit);
end mico;

architecture functional of mico is

  component mem_prog is                 -- no arquivo mem.vhd
  port (ender : in  reg6;
    instr : out reg32);
  end component mem_prog;

  component display is                  -- neste arquivo
  port (rst,clk : in bit;
    enable  : in bit;
    data    : in reg32);
  end component display;

  component ULA is                      -- neste arquivo
  port (fun : in reg4;
    alfa,beta : in  reg32;
    gama      : out reg32);
  end component ULA;

  component R is                        -- neste arquivo
  port (clk         : in  bit;
    wr_en       : in  bit;
    r_a,r_b,r_c : in  reg4;
    A,B         : out reg32;
    C           : in  reg32);
  end component R;

  component RAM is                      -- neste arquivo
  port (rst, clk : in  bit;
          sel      : in  bit;           -- ativo em 1
          wr       : in  bit;           -- ativo em 1
          ender    : in  reg16;
          data_inp : in  reg32;
          data_out : out reg32);
  end component RAM;

------------------------------------------------------------------------------------------------------
component inv is
generic (prop : time := t_inv);
port(A : in  bit;
 S : out  bit);
end component inv;

-----------------------------------------------------------------------------------------------------
component count32up is
port(rel, rst, ld, en: in  bit;
  D:               in  reg32;
  Q:               out reg32);
end component count32up;


-----------------------------------------------------------------------------------------------------
type t_control_type is record
    extZero    : bit;      -- estende com zero=1, com sinal=0
    selBeta    : bit;      -- seleciona fonte para entrada B da ULA
    wr_display : bit;      -- atualiza display=1
    selNxtIP   : bit;      -- seleciona fonte do incremento do IP
    wr_reg     : bit;      -- atualiza banco de registradores
    selC       : bit;      -- seleciona fonte da escrita no reg destino
    mem_sel    : bit;      -- habilita acesso a RAM
    mem_wr     : bit;      -- habilita escrita na RAM
    end record;

    type t_control_mem is array (0 to 15) of t_control_type;

    constant ctrl_table : t_control_mem := (
  --extZ sBeta wrD sIP wrR selC  M_sel M_wr
    ('0','0', '0', '0','0', '0', '0', '0'),            -- NOP
    ('0','0', '0', '0','1', '0', '0', '0'),            -- ADD
    ('0','0', '0', '0','1', '0', '0', '0'),            -- SUB
    ('0','0', '0', '0','1', '0', '0', '0'),            -- MUL
    ('0','0', '0', '0','1', '0', '0', '0'),            -- AND
    ('0','0', '0', '0','1', '0', '0', '0'),            -- OR
    ('0','0', '0', '0','1', '0', '0', '0'),            -- XOR
    ('0','0', '0', '0','1', '0', '0', '0'),            -- NOT
    ('1','1', '0', '0','1', '0', '0', '0'),            -- ORI
    ('1','1', '0', '0','1', '0', '0', '0'),            -- ADDI
    ('1','1', '0', '0','1', '1', '1', '0'),            -- LD
    ('1','0', '0', '0','0', '0', '0', '1'),            -- ST
    ('1','0', '1', '0','0', '0', '0', '0'),            -- SHOW
    ('1','0', '0', '1','0', '0', '0', '0'),            -- JUMP
    ('1','0', '0', '1','0', '0', '0', '0'),            -- BRANCH
    ('1','0', '0', '1','0', '0', '0', '0'));           -- HALT

    constant HALT : bit_vector := x"f";


    signal extZero, selBeta, wr_display, selNxtIP, wr_reg, selC : bit;
    signal mem_sel, mem_wr, cont_en: bit;

    signal instr, A, B, C, beta, extended, ula_D, mem_D,p, n_ip, a_ip : reg32;
    signal Asel,Bsel,Csel : reg4;
    signal this  : t_control_type;
    signal const, ip, ext_zeros, ext_sinal : reg16;
    signal opcode : reg4;
    signal i_opcode : natural range 0 to 15;

begin  -- functional

  -- Contador ip
  U_encont: inv generic map (t_inv) port map (selNxtIP, cont_en);
  a_ip <= x"0000" & ip;
  U_cont32 : count32up port map(clk, rst, cont_en, selNxtIP, a_ip, n_ip);

  -- Mux ip
  ip <= n_ip(15 downto 0) when selNxtIP = '0' else
  const;

  -- memoria de programa contem somente 64 palavras
  U_mem_prog: mem_prog port map(ip(5 downto 0), instr);

  opcode <= instr(31 downto 28);
  i_opcode <= BV2INT4(opcode);          -- indice do vetor DEVE ser inteiro
  const    <= instr(15 downto 0);

  this <= ctrl_table(i_opcode);         -- sinais de controle

  extZero    <= this.extZero;
  selBeta    <= this.selBeta;
  wr_display <= this.wr_display;
  selNxtIP   <= this.selNxtIP;
  wr_reg     <= this.wr_reg;
  selC       <= this.selC;
  mem_sel    <= this.mem_sel;
  mem_wr     <= this.mem_wr;

  -- Extensores recebem os valores iniciais
  ext_zeros <= x"0000";
  ext_sinal <= x"1111";

  -- Constante extendida recebe extensores no inicio
  extended <= ext_sinal(15 downto 0) & const(15 downto 0) when extZero = '1' and const(15) = '1' else
  ext_zeros(15 downto 0) & const(15 downto 0);

  -- Mux de beta
  beta <= B(31 downto 0) when selBeta = '0' else
    extended(31 downto 0);


  U_regs: R port map (clk, wr_reg,instr(27 downto 24),instr(23 downto 20),instr(19 downto 16),A,B,C);



  U_ULA: ULA port map (opcode,A(31 downto 0),B(31 downto 0) ,ula_D);



  U_mem: RAM port map (rst, clk, mem_sel, mem_wr,const,ula_D,p);

  C <= ula_D(31 downto 0) when selC = '0' else
    p(31 downto 0);

  -- nao altere esta linha
  U_display: display port map (rst, clk, wr_display, A);


  assert opcode /= HALT
  report LF & LF & "simulation halted: " &
  "ender = "&integer'image(BV2INT16(ip))&" = "&BV16HEX(ip)&LF
  severity failure;

  end functional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity ULA is
port (fun : in reg4;
  alfa,beta : in  reg32;
  gama      : out reg32);
end ULA;

architecture behaviour of ULA is

  component adderAdianta16 is    -- soma/subtração
  port(inpA, inpB : in reg16;
       outC : out reg16;
       vem  : in bit;
       vai  : out bit
       );
  end component adderAdianta16;

   component mult16x16 is  --multiplicação
   port(A, B : in  reg16;
        prod : out reg32);
   end component mult16x16;

   component and32 is --and
   port (A,B  : in reg32;
    Z : out reg32);
   end component and32;

    component or32 is --or
    port(A,B  : in  reg32;
     Z  : out reg32);
    end component or32;

   component xor32 is --xor
   port(A,B  : in  reg32;
     Z  : out reg32);
   end component xor32;

   component inv32 is --inversor
   port(A : in  reg32;
     Z  : out reg32);
   end component inv32;

    component shiftLeft32 is --move para esquerda
    port(A,B  : in  reg32;
     Z  : out reg32);
    end component shiftLeft32;

   component shiftRight32 is  --move pra direita
   port(A,B  : in  reg32;
    Z : out reg32);
   end component shiftRight32;

   component mux16 is
        port(sel  : in reg4;
          A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P : in reg32;
          S  : out reg32);
   end component mux16;

--signal
signal  add, sub, mult, and32out, or32out, xor32out, inv32out, s32right, s32left, omega: reg32;
signal  vaiAdd, vaiSub, vaiLowAdd, vaiLowSub : bit;

begin  -- behaviour

  -- soma
  U_somaLow: adderAdianta16 port map (alfa(15 downto 0), beta(15 downto 0), add(15 downto 0), '0',    vaiLowAdd);
  U_somaHigh: adderAdianta16 port map (alfa(31 downto 16), beta(31 downto 16), add(31 downto 16),   vaiLowAdd, vaiAdd);

  -- subtracao
  U_invSub: inv32 port map (beta(31 downto 0), omega(31 downto 0));
  U_subLow: adderAdianta16 port map (alfa(15 downto 0), omega(15 downto 0), sub(15 downto 0), '1',    vaiLowSub);
  U_subHigh: adderAdianta16 port map (alfa(31 downto 16), omega(31 downto 16), sub(31 downto 16),   vaiLowSub, vaiSub);

  -- multiplicacao
  U_mult: mult16x16 port map (alfa(15 downto 0), beta (15 downto 0), mult);

  -- and
  U_and: and32 port map(alfa(31 downto 0), beta(31 downto 0), and32out(31 downto 0));

  -- or
  U_or: or32 port map(alfa(31 downto 0), beta(31 downto 0), or32out(31 downto 0));

  -- xor
  U_xor: xor32 port map(alfa(31 downto 0), beta(31 downto 0), xor32out(31 downto 0));

  -- not
  U_not: inv32 port map (alfa(31 downto 0), inv32out(31 downto 0));

  -- deslocador para a direita
  -- U_right: shiftRight32 port map (alfa(31 downto 0), beta(31 downto 0), s32right(31 downto 0));

  -- deslocador para a esquerda
  -- U_Left: shiftLeft32 port map (alfa(31 downto 0), beta(31 downto 0), s32left(31 downto 0));

  U_mux: mux16 port map (fun, x"00000000", add, sub, mult, and32out, or32out, xor32out, inv32out, s32right, s32left, or32out, add,x"00000000",x"00000000",x"00000000",x"00000000", gama);

   end behaviour;

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity and32 is
  port(A,B : in  reg32;
       Z   : out reg32);
end and32;

<<<<<<< HEAD
architecture estrut of and32 is 
=======
architecture estrut of and32 is
>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
  component and2 is
    generic (prop : time);
    port(A,B : in bit; S : out bit);
  end component and2;
 begin
  U_and0:  and2  generic map (t_and2)  port map(A(0),B(0),Z(0));
  U_and1:  and2  generic map (t_and2)  port map(A(1),B(1),Z(1));
  U_and2:  and2  generic map (t_and2)  port map(A(2),B(2),Z(2));
  U_and3:  and2  generic map (t_and2)  port map(A(3),B(3),Z(3));
  U_and4:  and2  generic map (t_and2)  port map(A(4),B(4),Z(4));
  U_and5:  and2  generic map (t_and2)  port map(A(5),B(5),Z(5));
  U_and6:  and2  generic map (t_and2)  port map(A(6),B(6),Z(6));
  U_and7:  and2  generic map (t_and2)  port map(A(7),B(7),Z(7));
  U_and8:  and2  generic map (t_and2)  port map(A(8),B(8),Z(8));
  U_and9:  and2  generic map (t_and2)  port map(A(9),B(9),Z(9));
  U_and10:  and2  generic map (t_and2)  port map(A(10),B(10),Z(10));
  U_and11:  and2  generic map (t_and2)  port map(A(11),B(11),Z(11));
  U_and12:  and2  generic map (t_and2)  port map(A(12),B(12),Z(12));
  U_and13:  and2  generic map (t_and2)  port map(A(13),B(13),Z(13));
  U_and14:  and2  generic map (t_and2)  port map(A(14),B(14),Z(14));
  U_and15:  and2  generic map (t_and2)  port map(A(15),B(15),Z(15));
  U_and16:  and2  generic map (t_and2)  port map(A(16),B(16),Z(16));
  U_and17:  and2  generic map (t_and2)  port map(A(17),B(17),Z(17));
  U_and18:  and2  generic map (t_and2)  port map(A(18),B(18),Z(18));
  U_and19:  and2  generic map (t_and2)  port map(A(19),B(19),Z(19));
  U_and20:  and2  generic map (t_and2)  port map(A(20),B(20),Z(20));
  U_and21:  and2  generic map (t_and2)  port map(A(21),B(21),Z(21));
  U_and22:  and2  generic map (t_and2)  port map(A(22),B(22),Z(22));
  U_and23:  and2  generic map (t_and2)  port map(A(23),B(23),Z(23));
  U_and24:  and2  generic map (t_and2)  port map(A(24),B(24),Z(24));
  U_and25:  and2  generic map (t_and2)  port map(A(25),B(25),Z(25));
  U_and26:  and2  generic map (t_and2)  port map(A(26),B(26),Z(26));
  U_and27:  and2  generic map (t_and2)  port map(A(27),B(27),Z(27));
  U_and28:  and2  generic map (t_and2)  port map(A(28),B(28),Z(28));
  U_and29:  and2  generic map (t_and2)  port map(A(29),B(29),Z(29));
  U_and30:  and2  generic map (t_and2)  port map(A(30),B(30),Z(30));
  U_and31:  and2  generic map (t_and2)  port map(A(31),B(31),Z(31));

end architecture estrut;

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity or32 is
  port(A,B : in  reg32;
       Z   : out reg32);
end or32;

<<<<<<< HEAD
architecture estrut of or32 is 
=======
architecture estrut of or32 is
>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
  component or2 is
    generic (prop : time);
    port(A,B : in bit; S : out bit);
  end component or2;
 begin
  U_or0:  or2  generic map (t_or2)  port map(A(0),B(0),Z(0));
  U_or1:  or2  generic map (t_or2)  port map(A(1),B(1),Z(1));
  U_or2:  or2  generic map (t_or2)  port map(A(2),B(2),Z(2));
  U_or3:  or2  generic map (t_or2)  port map(A(3),B(3),Z(3));
  U_or4:  or2  generic map (t_or2)  port map(A(4),B(4),Z(4));
  U_or5:  or2  generic map (t_or2)  port map(A(5),B(5),Z(5));
  U_or6:  or2  generic map (t_or2)  port map(A(6),B(6),Z(6));
  U_or7:  or2  generic map (t_or2)  port map(A(7),B(7),Z(7));
  U_or8:  or2  generic map (t_or2)  port map(A(8),B(8),Z(8));
  U_or9:  or2  generic map (t_or2)  port map(A(9),B(9),Z(9));
  U_or10:  or2  generic map (t_or2)  port map(A(10),B(10),Z(10));
  U_or11:  or2  generic map (t_or2)  port map(A(11),B(11),Z(11));
  U_or12:  or2  generic map (t_or2)  port map(A(12),B(12),Z(12));
  U_or13:  or2  generic map (t_or2)  port map(A(13),B(13),Z(13));
  U_or14:  or2  generic map (t_or2)  port map(A(14),B(14),Z(14));
  U_or15:  or2  generic map (t_or2)  port map(A(15),B(15),Z(15));
  U_or16:  or2  generic map (t_or2)  port map(A(16),B(16),Z(16));
  U_or17:  or2  generic map (t_or2)  port map(A(17),B(17),Z(17));
  U_or18:  or2  generic map (t_or2)  port map(A(18),B(18),Z(18));
  U_or19:  or2  generic map (t_or2)  port map(A(19),B(19),Z(19));
  U_or20:  or2  generic map (t_or2)  port map(A(20),B(20),Z(20));
  U_or21:  or2  generic map (t_or2)  port map(A(21),B(21),Z(21));
  U_or22:  or2  generic map (t_or2)  port map(A(22),B(22),Z(22));
  U_or23:  or2  generic map (t_or2)  port map(A(23),B(23),Z(23));
  U_or24:  or2  generic map (t_or2)  port map(A(24),B(24),Z(24));
  U_or25:  or2  generic map (t_or2)  port map(A(25),B(25),Z(25));
  U_or26:  or2  generic map (t_or2)  port map(A(26),B(26),Z(26));
  U_or27:  or2  generic map (t_or2)  port map(A(27),B(27),Z(27));
  U_or28:  or2  generic map (t_or2)  port map(A(28),B(28),Z(28));
  U_or29:  or2  generic map (t_or2)  port map(A(29),B(29),Z(29));
  U_or30:  or2  generic map (t_or2)  port map(A(30),B(30),Z(30));
  U_or31:  or2  generic map (t_or2)  port map(A(31),B(31),Z(31));

end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity xor32 is
  port(A,B : in  reg32;
       Z   : out reg32);
end xor32;

<<<<<<< HEAD
architecture estrut of xor32 is 
=======
architecture estrut of xor32 is
>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
  component xor2 is
    port(A,B : in bit; S : out bit);
  end component xor2;
 begin

  U_xor0:  xor2  port map(A(0),B(0),Z(0));
  U_xor1:  xor2  port map(A(1),B(1),Z(1));
  U_xor2:  xor2  port map(A(2),B(2),Z(2));
  U_xor3:  xor2  port map(A(3),B(3),Z(3));
  U_xor4:  xor2  port map(A(4),B(4),Z(4));
  U_xor5:  xor2  port map(A(5),B(5),Z(5));
  U_xor6:  xor2  port map(A(6),B(6),Z(6));
  U_xor7:  xor2  port map(A(7),B(7),Z(7));
  U_xor8:  xor2  port map(A(8),B(8),Z(8));
  U_xor9:  xor2  port map(A(9),B(9),Z(9));
  U_xor10:  xor2  port map(A(10),B(10),Z(10));
  U_xor11:  xor2  port map(A(11),B(11),Z(11));
  U_xor12:  xor2  port map(A(12),B(12),Z(12));
  U_xor13:  xor2  port map(A(13),B(13),Z(13));
  U_xor14:  xor2  port map(A(14),B(14),Z(14));
  U_xor15:  xor2  port map(A(15),B(15),Z(15));
  U_xor16:  xor2  port map(A(16),B(16),Z(16));
  U_xor17:  xor2  port map(A(17),B(17),Z(17));
  U_xor18:  xor2  port map(A(18),B(18),Z(18));
  U_xor19:  xor2  port map(A(19),B(19),Z(19));
  U_xor20:  xor2  port map(A(20),B(20),Z(20));
  U_xor21:  xor2  port map(A(21),B(21),Z(21));
  U_xor22:  xor2  port map(A(22),B(22),Z(22));
  U_xor23:  xor2  port map(A(23),B(23),Z(23));
  U_xor24:  xor2  port map(A(24),B(24),Z(24));
  U_xor25:  xor2  port map(A(25),B(25),Z(25));
  U_xor26:  xor2  port map(A(26),B(26),Z(26));
  U_xor27:  xor2  port map(A(27),B(27),Z(27));
  U_xor28:  xor2  port map(A(28),B(28),Z(28));
  U_xor29:  xor2  port map(A(29),B(29),Z(29));
  U_xor30:  xor2  port map(A(30),B(30),Z(30));
  U_xor31:  xor2  port map(A(31),B(31),Z(31));

end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity inv32 is
  port(A : in  reg32;
       Z   : out reg32);
end inv32;

<<<<<<< HEAD
architecture estrut of inv32 is 
=======
architecture estrut of inv32 is
>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
  component inv is
    generic (prop : time);
    port(A: in bit; S : out bit);
  end component inv;
 begin
  U_inv0:  inv  generic map (t_inv)  port map(A(0),Z(0));
  U_inv1:  inv  generic map (t_inv)  port map(A(1),Z(1));
  U_inv2:  inv  generic map (t_inv)  port map(A(2),Z(2));
  U_inv3:  inv  generic map (t_inv)  port map(A(3),Z(3));
  U_inv4:  inv  generic map (t_inv)  port map(A(4),Z(4));
  U_inv5:  inv  generic map (t_inv)  port map(A(5),Z(5));
  U_inv6:  inv  generic map (t_inv)  port map(A(6),Z(6));
  U_inv7:  inv  generic map (t_inv)  port map(A(7),Z(7));
  U_inv8:  inv  generic map (t_inv)  port map(A(8),Z(8));
  U_inv9:  inv  generic map (t_inv)  port map(A(9),Z(9));
  U_inv10:  inv  generic map (t_inv)  port map(A(10),Z(10));
  U_inv11:  inv  generic map (t_inv)  port map(A(11),Z(11));
  U_inv12:  inv  generic map (t_inv)  port map(A(12),Z(12));
  U_inv13:  inv  generic map (t_inv)  port map(A(13),Z(13));
  U_inv14:  inv  generic map (t_inv)  port map(A(14),Z(14));
  U_inv15:  inv  generic map (t_inv)  port map(A(15),Z(15));
  U_inv16:  inv  generic map (t_inv)  port map(A(16),Z(16));
  U_inv17:  inv  generic map (t_inv)  port map(A(17),Z(17));
  U_inv18:  inv  generic map (t_inv)  port map(A(18),Z(18));
  U_inv19:  inv  generic map (t_inv)  port map(A(19),Z(19));
  U_inv20:  inv  generic map (t_inv)  port map(A(20),Z(20));
  U_inv21:  inv  generic map (t_inv)  port map(A(21),Z(21));
  U_inv22:  inv  generic map (t_inv)  port map(A(22),Z(22));
  U_inv23:  inv  generic map (t_inv)  port map(A(23),Z(23));
  U_inv24:  inv  generic map (t_inv)  port map(A(24),Z(24));
  U_inv25:  inv  generic map (t_inv)  port map(A(25),Z(25));
  U_inv26:  inv  generic map (t_inv)  port map(A(26),Z(26));
  U_inv27:  inv  generic map (t_inv)  port map(A(27),Z(27));
  U_inv28:  inv  generic map (t_inv)  port map(A(28),Z(28));
  U_inv29:  inv  generic map (t_inv)  port map(A(29),Z(29));
  U_inv30:  inv  generic map (t_inv)  port map(A(30),Z(30));
  U_inv31:  inv  generic map (t_inv)  port map(A(31),Z(31));

end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity shiftLeft1 is
  port(A,B : in  reg32;
       Z   : out reg32);
end shiftLeft1;

<<<<<<< HEAD
architecture estrut of shiftLeft1 is 
  
  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;
  
 begin  
=======
architecture estrut of shiftLeft1 is

  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;

 begin
>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
    U_mux0: mux2 port map (A(0), '0', B(0),Z(0));
    U_mux1: mux2 port map (A(1), A(0), B(0),Z(1));
    U_mux2: mux2 port map (A(2), A(1), B(0),Z(2));
    U_mux3: mux2 port map (A(3), A(2), B(0),Z(3));
    U_mux4: mux2 port map (A(4), A(3), B(0),Z(4));
    U_mux5: mux2 port map (A(5), A(4), B(0),Z(5));
    U_mux6: mux2 port map (A(6), A(5), B(0),Z(6));
    U_mux7: mux2 port map (A(7), A(6), B(0),Z(7));
    U_mux8: mux2 port map (A(8), A(7), B(0),Z(8));
    U_mux9: mux2 port map (A(9), A(8), B(0),Z(9));
    U_mux10: mux2 port map (A(10), A(9), B(0),Z(10));
    U_mux11: mux2 port map (A(11), A(10), B(0),Z(11));
    U_mux12: mux2 port map (A(12), A(11), B(0),Z(12));
    U_mux13: mux2 port map (A(13), A(12), B(0),Z(13));
    U_mux14: mux2 port map (A(14), A(13), B(0),Z(14));
    U_mux15: mux2 port map (A(15), A(14), B(0),Z(15));
    U_mux16: mux2 port map (A(16), A(15), B(0),Z(16));
    U_mux17: mux2 port map (A(17), A(16), B(0),Z(17));
    U_mux18: mux2 port map (A(18), A(17), B(0),Z(18));
    U_mux19: mux2 port map (A(19), A(18), B(0),Z(19));
    U_mux20: mux2 port map (A(20), A(19), B(0),Z(20));
    U_mux21: mux2 port map (A(21), A(20), B(0),Z(21));
    U_mux22: mux2 port map (A(22), A(21), B(0),Z(22));
    U_mux23: mux2 port map (A(23), A(22), B(0),Z(23));
    U_mux24: mux2 port map (A(24), A(23), B(0),Z(24));
    U_mux25: mux2 port map (A(25), A(24), B(0),Z(25));
    U_mux26: mux2 port map (A(26), A(25), B(0),Z(26));
    U_mux27: mux2 port map (A(27), A(26), B(0),Z(27));
    U_mux28: mux2 port map (A(28), A(27), B(0),Z(28));
    U_mux29: mux2 port map (A(29), A(28), B(0),Z(29));
    U_mux30: mux2 port map (A(30), A(29), B(0),Z(30));
    U_mux31: mux2 port map (A(31), A(30), B(0),Z(31));

end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity shiftLeft2 is
  port(A,B : in  reg32;
       Z   : out reg32);
end shiftLeft2;

<<<<<<< HEAD
architecture estrut of shiftLeft2 is 
  
  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;
  
 begin  
=======
architecture estrut of shiftLeft2 is

  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;

 begin
>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
    U_mux0: mux2 port map (A(0), '0', B(1),Z(0));
    U_mux1: mux2 port map (A(1), '0', B(1),Z(1));
    U_mux2: mux2 port map (A(2), A(0), B(1),Z(2));
    U_mux3: mux2 port map (A(3), A(1), B(1),Z(3));
    U_mux4: mux2 port map (A(4), A(2), B(1),Z(4));
    U_mux5: mux2 port map (A(5), A(3), B(1),Z(5));
    U_mux6: mux2 port map (A(6), A(4), B(1),Z(6));
    U_mux7: mux2 port map (A(7), A(5), B(1),Z(7));
    U_mux8: mux2 port map (A(8), A(6), B(1),Z(8));
    U_mux9: mux2 port map (A(9), A(7), B(1),Z(9));
    U_mux10: mux2 port map (A(10), A(8), B(1),Z(10));
    U_mux11: mux2 port map (A(11), A(9), B(1),Z(11));
    U_mux12: mux2 port map (A(12), A(10), B(1),Z(12));
    U_mux13: mux2 port map (A(13), A(11), B(1),Z(13));
    U_mux14: mux2 port map (A(14), A(12), B(1),Z(14));
    U_mux15: mux2 port map (A(15), A(13), B(1),Z(15));
    U_mux16: mux2 port map (A(16), A(14), B(1),Z(16));
    U_mux17: mux2 port map (A(17), A(15), B(1),Z(17));
    U_mux18: mux2 port map (A(18), A(16), B(1),Z(18));
    U_mux19: mux2 port map (A(19), A(17), B(1),Z(19));
    U_mux20: mux2 port map (A(20), A(18), B(1),Z(20));
    U_mux21: mux2 port map (A(21), A(19), B(1),Z(21));
    U_mux22: mux2 port map (A(22), A(20), B(1),Z(22));
    U_mux23: mux2 port map (A(23), A(21), B(1),Z(23));
    U_mux24: mux2 port map (A(24), A(22), B(1),Z(24));
    U_mux25: mux2 port map (A(25), A(23), B(1),Z(25));
    U_mux26: mux2 port map (A(26), A(24), B(1),Z(26));
    U_mux27: mux2 port map (A(27), A(25), B(1),Z(27));
    U_mux28: mux2 port map (A(28), A(26), B(1),Z(28));
    U_mux29: mux2 port map (A(29), A(27), B(1),Z(29));
    U_mux30: mux2 port map (A(30), A(28), B(1),Z(30));
    U_mux31: mux2 port map (A(31), A(29), B(1),Z(31));
<<<<<<< HEAD
    
=======

>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity shiftLeft4 is
  port(A,B : in  reg32;
       Z   : out reg32);
end shiftLeft4;

<<<<<<< HEAD
architecture estrut of shiftLeft4 is 
  
  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;
  
 begin  
=======
architecture estrut of shiftLeft4 is

  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;

 begin
>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
    U_mux0: mux2 port map (A(0), '0', B(2),Z(0));
    U_mux1: mux2 port map (A(1), '0', B(2),Z(1));
    U_mux2: mux2 port map (A(2), '0', B(2),Z(2));
    U_mux3: mux2 port map (A(3), '0', B(2),Z(3));
    U_mux4: mux2 port map (A(4), A(0), B(2),Z(4));
    U_mux5: mux2 port map (A(5), A(1), B(2),Z(5));
    U_mux6: mux2 port map (A(6), A(2), B(2),Z(6));
    U_mux7: mux2 port map (A(7), A(3), B(2),Z(7));
    U_mux8: mux2 port map (A(8), A(4), B(2),Z(8));
    U_mux9: mux2 port map (A(9), A(5), B(2),Z(9));
    U_mux10: mux2 port map (A(10), A(6), B(2),Z(10));
    U_mux11: mux2 port map (A(11), A(7), B(2),Z(11));
    U_mux12: mux2 port map (A(12), A(8), B(2),Z(12));
    U_mux13: mux2 port map (A(13), A(9), B(2),Z(13));
    U_mux14: mux2 port map (A(14), A(10), B(2),Z(14));
    U_mux15: mux2 port map (A(15), A(11), B(2),Z(15));
    U_mux16: mux2 port map (A(16), A(12), B(2),Z(16));
    U_mux17: mux2 port map (A(17), A(13), B(2),Z(17));
    U_mux18: mux2 port map (A(18), A(14), B(2),Z(18));
    U_mux19: mux2 port map (A(19), A(15), B(2),Z(19));
    U_mux20: mux2 port map (A(20), A(16), B(2),Z(20));
    U_mux21: mux2 port map (A(21), A(17), B(2),Z(21));
    U_mux22: mux2 port map (A(22), A(18), B(2),Z(22));
    U_mux23: mux2 port map (A(23), A(19), B(2),Z(23));
    U_mux24: mux2 port map (A(24), A(20), B(2),Z(24));
    U_mux25: mux2 port map (A(25), A(21), B(2),Z(25));
    U_mux26: mux2 port map (A(26), A(22), B(2),Z(26));
    U_mux27: mux2 port map (A(27), A(23), B(2),Z(27));
    U_mux28: mux2 port map (A(28), A(24), B(2),Z(28));
    U_mux29: mux2 port map (A(29), A(25), B(2),Z(29));
    U_mux30: mux2 port map (A(30), A(26), B(2),Z(30));
    U_mux31: mux2 port map (A(31), A(27), B(2),Z(31));
<<<<<<< HEAD
    
=======

>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity shiftLeft8 is
  port(A,B : in  reg32;
       Z   : out reg32);
end shiftLeft8;

<<<<<<< HEAD
architecture estrut of shiftLeft8 is 
  
  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;
  
 begin  
=======
architecture estrut of shiftLeft8 is

  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;

 begin
>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
    U_mux0: mux2 port map (A(0), '0', B(3),Z(0));
    U_mux1: mux2 port map (A(1), '0', B(3),Z(1));
    U_mux2: mux2 port map (A(2), '0', B(3),Z(2));
    U_mux3: mux2 port map (A(3), '0', B(3),Z(3));
    U_mux4: mux2 port map (A(4), '0', B(3),Z(4));
    U_mux5: mux2 port map (A(5), '0', B(3),Z(5));
    U_mux6: mux2 port map (A(6), '0', B(3),Z(6));
    U_mux7: mux2 port map (A(7), '0', B(3),Z(7));
    U_mux8: mux2 port map (A(8), A(0), B(3),Z(8));
    U_mux9: mux2 port map (A(9), A(1), B(3),Z(9));
    U_mux10: mux2 port map (A(10), A(2), B(3),Z(10));
    U_mux11: mux2 port map (A(11), A(3), B(3),Z(11));
    U_mux12: mux2 port map (A(12), A(4), B(3),Z(12));
    U_mux13: mux2 port map (A(13), A(5), B(3),Z(13));
    U_mux14: mux2 port map (A(14), A(6), B(3),Z(14));
    U_mux15: mux2 port map (A(15), A(7), B(3),Z(15));
    U_mux16: mux2 port map (A(16), A(8), B(3),Z(16));
    U_mux17: mux2 port map (A(17), A(9), B(3),Z(17));
    U_mux18: mux2 port map (A(18), A(10), B(3),Z(18));
    U_mux19: mux2 port map (A(19), A(11), B(3),Z(19));
    U_mux20: mux2 port map (A(20), A(12), B(3),Z(20));
    U_mux21: mux2 port map (A(21), A(13), B(3),Z(21));
    U_mux22: mux2 port map (A(22), A(14), B(3),Z(22));
    U_mux23: mux2 port map (A(23), A(15), B(3),Z(23));
    U_mux24: mux2 port map (A(24), A(16), B(3),Z(24));
    U_mux25: mux2 port map (A(25), A(17), B(3),Z(25));
    U_mux26: mux2 port map (A(26), A(18), B(3),Z(26));
    U_mux27: mux2 port map (A(27), A(19), B(3),Z(27));
    U_mux28: mux2 port map (A(28), A(20), B(3),Z(28));
    U_mux29: mux2 port map (A(29), A(21), B(3),Z(29));
    U_mux30: mux2 port map (A(30), A(22), B(3),Z(30));
    U_mux31: mux2 port map (A(31), A(23), B(3),Z(31));
<<<<<<< HEAD
    
=======

>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity shiftLeft16 is
  port(A,B : in  reg32;
       Z   : out reg32);
end shiftLeft16;

<<<<<<< HEAD
architecture estrut of shiftLeft16 is 
  
  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;
  
 begin  
=======
architecture estrut of shiftLeft16 is

  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;

 begin
>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
    U_mux0: mux2 port map (A(0), '0', B(4),Z(0));
    U_mux1: mux2 port map (A(1), '0', B(4),Z(1));
    U_mux2: mux2 port map (A(2), '0', B(4),Z(2));
    U_mux3: mux2 port map (A(3), '0', B(4),Z(3));
    U_mux4: mux2 port map (A(4), '0', B(4),Z(4));
    U_mux5: mux2 port map (A(5), '0', B(4),Z(5));
    U_mux6: mux2 port map (A(6), '0', B(4),Z(6));
    U_mux7: mux2 port map (A(7), '0', B(4),Z(7));
    U_mux8: mux2 port map (A(8), '0', B(4),Z(8));
    U_mux9: mux2 port map (A(9), '0', B(4),Z(9));
    U_mux10: mux2 port map (A(10), '0', B(4),Z(10));
    U_mux11: mux2 port map (A(11), '0', B(4),Z(11));
    U_mux12: mux2 port map (A(12), '0', B(4),Z(12));
    U_mux13: mux2 port map (A(13), '0', B(4),Z(13));
    U_mux14: mux2 port map (A(14), '0', B(4),Z(14));
    U_mux15: mux2 port map (A(15), '0', B(4),Z(15));
    U_mux16: mux2 port map (A(16), A(0), B(4),Z(16));
    U_mux17: mux2 port map (A(17), A(1), B(4),Z(17));
    U_mux18: mux2 port map (A(18), A(2), B(4),Z(18));
    U_mux19: mux2 port map (A(19), A(3), B(4),Z(19));
    U_mux20: mux2 port map (A(20), A(4), B(4),Z(20));
    U_mux21: mux2 port map (A(21), A(5), B(4),Z(21));
    U_mux22: mux2 port map (A(22), A(6), B(4),Z(22));
    U_mux23: mux2 port map (A(23), A(7), B(4),Z(23));
    U_mux24: mux2 port map (A(24), A(8), B(4),Z(24));
    U_mux25: mux2 port map (A(25), A(9), B(4),Z(25));
    U_mux26: mux2 port map (A(26), A(10), B(4),Z(26));
    U_mux27: mux2 port map (A(27), A(11), B(4),Z(27));
    U_mux28: mux2 port map (A(28), A(12), B(4),Z(28));
    U_mux29: mux2 port map (A(29), A(13), B(4),Z(29));
    U_mux30: mux2 port map (A(30), A(14), B(4),Z(30));
    U_mux31: mux2 port map (A(31), A(15), B(4),Z(31));
<<<<<<< HEAD
    
=======

>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity shiftLeft32 is
  port(A,B : in  reg32;
       Z   : out reg32);
end shiftLeft32;

<<<<<<< HEAD
architecture estrut of shiftLeft32 is 
  
  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;
  
  component shiftLeft1 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftLeft1;
  
  component shiftLeft2 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftLeft2;
  
  component shiftLeft4 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftLeft4;
  
  component shiftLeft8 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftLeft8;
  
  component shiftLeft16 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftLeft16;
  
  signal J,K,L,M,N : reg32;
 begin  
=======
architecture estrut of shiftLeft32 is

  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;

  component shiftLeft1 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftLeft1;

  component shiftLeft2 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftLeft2;

  component shiftLeft4 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftLeft4;

  component shiftLeft8 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftLeft8;

  component shiftLeft16 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftLeft16;

  signal J,K,L,M,N : reg32;
 begin
>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
    U_shift1: shiftLeft1 port map (A,B,J);
    U_shift2: shiftLeft2 port map (J,B,K);
    U_shift4: shiftLeft4 port map (K,B,L);
    U_shift8: shiftLeft8 port map (L,B,M);
    U_shift16: shiftLeft16 port map (M,B,N);
<<<<<<< HEAD
    
=======

>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
    Z <= N when B <= x"20" else (x"00000000");
end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity shiftRight1 is
  port(A,B : in  reg32;
       Z   : out reg32);
end shiftRight1;

<<<<<<< HEAD
architecture estrut of shiftRight1 is 
  
  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;
  
 begin  
=======
architecture estrut of shiftRight1 is

  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;

 begin
>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
    U_mux0: mux2 port map (A(0), A(1), B(0),Z(0));
    U_mux1: mux2 port map (A(1), A(2), B(0),Z(1));
    U_mux2: mux2 port map (A(2), A(3), B(0),Z(2));
    U_mux3: mux2 port map (A(3), A(4), B(0),Z(3));
    U_mux4: mux2 port map (A(4), A(5), B(0),Z(4));
    U_mux5: mux2 port map (A(5), A(6), B(0),Z(5));
    U_mux6: mux2 port map (A(6), A(7), B(0),Z(6));
    U_mux7: mux2 port map (A(7), A(8), B(0),Z(7));
    U_mux8: mux2 port map (A(8), A(9), B(0),Z(8));
    U_mux9: mux2 port map (A(9), A(10), B(0),Z(9));
    U_mux10: mux2 port map (A(10), A(11), B(0),Z(10));
    U_mux11: mux2 port map (A(11), A(12), B(0),Z(11));
    U_mux12: mux2 port map (A(12), A(13), B(0),Z(12));
    U_mux13: mux2 port map (A(13), A(14), B(0),Z(13));
    U_mux14: mux2 port map (A(14), A(15), B(0),Z(14));
    U_mux15: mux2 port map (A(15), A(16), B(0),Z(15));
    U_mux16: mux2 port map (A(16), A(17), B(0),Z(16));
    U_mux17: mux2 port map (A(17), A(18), B(0),Z(17));
    U_mux18: mux2 port map (A(18), A(19), B(0),Z(18));
    U_mux19: mux2 port map (A(19), A(20), B(0),Z(19));
    U_mux20: mux2 port map (A(20), A(21), B(0),Z(20));
    U_mux21: mux2 port map (A(21), A(22), B(0),Z(21));
    U_mux22: mux2 port map (A(22), A(23), B(0),Z(22));
    U_mux23: mux2 port map (A(23), A(24), B(0),Z(23));
    U_mux24: mux2 port map (A(24), A(25), B(0),Z(24));
    U_mux25: mux2 port map (A(25), A(26), B(0),Z(25));
    U_mux26: mux2 port map (A(26), A(27), B(0),Z(26));
    U_mux27: mux2 port map (A(27), A(28), B(0),Z(27));
    U_mux28: mux2 port map (A(28), A(29), B(0),Z(28));
    U_mux29: mux2 port map (A(29), A(30), B(0),Z(29));
    U_mux30: mux2 port map (A(30), A(31), B(0),Z(30));
    U_mux31: mux2 port map (A(31), '0', B(0),Z(31));

end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity shiftRight2 is
  port(A,B : in  reg32;
       Z   : out reg32);
end shiftRight2;

<<<<<<< HEAD
architecture estrut of shiftRight2 is 
  
  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;
  
 begin  
=======
architecture estrut of shiftRight2 is

  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;

 begin
>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
    U_mux0: mux2 port map (A(0), A(2), B(0),Z(0));
    U_mux1: mux2 port map (A(1), A(3), B(0),Z(1));
    U_mux2: mux2 port map (A(2), A(4), B(0),Z(2));
    U_mux3: mux2 port map (A(3), A(5), B(0),Z(3));
    U_mux4: mux2 port map (A(4), A(6), B(0),Z(4));
    U_mux5: mux2 port map (A(5), A(7), B(0),Z(5));
    U_mux6: mux2 port map (A(6), A(8), B(0),Z(6));
    U_mux7: mux2 port map (A(7), A(9), B(0),Z(7));
    U_mux8: mux2 port map (A(8), A(10), B(0),Z(8));
    U_mux9: mux2 port map (A(9), A(11), B(0),Z(9));
    U_mux10: mux2 port map (A(10), A(12), B(0),Z(10));
    U_mux11: mux2 port map (A(11), A(13), B(0),Z(11));
    U_mux12: mux2 port map (A(12), A(14), B(0),Z(12));
    U_mux13: mux2 port map (A(13), A(15), B(0),Z(13));
    U_mux14: mux2 port map (A(14), A(16), B(0),Z(14));
    U_mux15: mux2 port map (A(15), A(17), B(0),Z(15));
    U_mux16: mux2 port map (A(16), A(18), B(0),Z(16));
    U_mux17: mux2 port map (A(17), A(19), B(0),Z(17));
    U_mux18: mux2 port map (A(18), A(20), B(0),Z(18));
    U_mux19: mux2 port map (A(19), A(21), B(0),Z(19));
    U_mux20: mux2 port map (A(20), A(22), B(0),Z(20));
    U_mux21: mux2 port map (A(21), A(23), B(0),Z(21));
    U_mux22: mux2 port map (A(22), A(24), B(0),Z(22));
    U_mux23: mux2 port map (A(23), A(25), B(0),Z(23));
    U_mux24: mux2 port map (A(24), A(26), B(0),Z(24));
    U_mux25: mux2 port map (A(25), A(27), B(0),Z(25));
    U_mux26: mux2 port map (A(26), A(28), B(0),Z(26));
    U_mux27: mux2 port map (A(27), A(29), B(0),Z(27));
    U_mux28: mux2 port map (A(28), A(30), B(0),Z(28));
    U_mux29: mux2 port map (A(29), A(31), B(0),Z(29));
    U_mux30: mux2 port map (A(30), '0', B(0),Z(30));
    U_mux31: mux2 port map (A(31), '0', B(0),Z(31));
<<<<<<< HEAD
    
=======

>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity shiftRight4 is
  port(A,B : in  reg32;
       Z   : out reg32);
end shiftRight4;

<<<<<<< HEAD
architecture estrut of shiftRight4 is 
  
  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;
  
 begin  
=======
architecture estrut of shiftRight4 is

  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;

 begin
>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
    U_mux0: mux2 port map (A(0), A(4), B(0),Z(0));
    U_mux1: mux2 port map (A(1), A(5), B(0),Z(1));
    U_mux2: mux2 port map (A(2), A(6), B(0),Z(2));
    U_mux3: mux2 port map (A(3), A(7), B(0),Z(3));
    U_mux4: mux2 port map (A(4), A(8), B(0),Z(4));
    U_mux5: mux2 port map (A(5), A(9), B(0),Z(5));
    U_mux6: mux2 port map (A(6), A(10), B(0),Z(6));
    U_mux7: mux2 port map (A(7), A(11), B(0),Z(7));
    U_mux8: mux2 port map (A(8), A(12), B(0),Z(8));
    U_mux9: mux2 port map (A(9), A(13), B(0),Z(9));
    U_mux10: mux2 port map (A(10), A(14), B(0),Z(10));
    U_mux11: mux2 port map (A(11), A(15), B(0),Z(11));
    U_mux12: mux2 port map (A(12), A(16), B(0),Z(12));
    U_mux13: mux2 port map (A(13), A(17), B(0),Z(13));
    U_mux14: mux2 port map (A(14), A(18), B(0),Z(14));
    U_mux15: mux2 port map (A(15), A(19), B(0),Z(15));
    U_mux16: mux2 port map (A(16), A(20), B(0),Z(16));
    U_mux17: mux2 port map (A(17), A(21), B(0),Z(17));
    U_mux18: mux2 port map (A(18), A(22), B(0),Z(18));
    U_mux19: mux2 port map (A(19), A(23), B(0),Z(19));
    U_mux20: mux2 port map (A(20), A(24), B(0),Z(20));
    U_mux21: mux2 port map (A(21), A(25), B(0),Z(21));
    U_mux22: mux2 port map (A(22), A(26), B(0),Z(22));
    U_mux23: mux2 port map (A(23), A(27), B(0),Z(23));
    U_mux24: mux2 port map (A(24), A(28), B(0),Z(24));
    U_mux25: mux2 port map (A(25), A(29), B(0),Z(25));
    U_mux26: mux2 port map (A(26), A(30), B(0),Z(26));
    U_mux27: mux2 port map (A(27), A(31), B(0),Z(27));
    U_mux28: mux2 port map (A(28), '0', B(0),Z(28));
    U_mux29: mux2 port map (A(29), '0', B(0),Z(29));
    U_mux30: mux2 port map (A(30), '0', B(0),Z(30));
    U_mux31: mux2 port map (A(31), '0', B(0),Z(31));
<<<<<<< HEAD
    
=======

>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity shiftRight8 is
  port(A,B : in  reg32;
       Z   : out reg32);
end shiftRight8;

<<<<<<< HEAD
architecture estrut of shiftRight8 is 
  
  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;
  
 begin  
=======
architecture estrut of shiftRight8 is

  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;

 begin
>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
    U_mux0: mux2 port map (A(0), A(8), B(0),Z(0));
    U_mux1: mux2 port map (A(1), A(9), B(0),Z(1));
    U_mux2: mux2 port map (A(2), A(10), B(0),Z(2));
    U_mux3: mux2 port map (A(3), A(11), B(0),Z(3));
    U_mux4: mux2 port map (A(4), A(12), B(0),Z(4));
    U_mux5: mux2 port map (A(5), A(13), B(0),Z(5));
    U_mux6: mux2 port map (A(6), A(14), B(0),Z(6));
    U_mux7: mux2 port map (A(7), A(15), B(0),Z(7));
    U_mux8: mux2 port map (A(8), A(16), B(0),Z(8));
    U_mux9: mux2 port map (A(9), A(17), B(0),Z(9));
    U_mux10: mux2 port map (A(10), A(18), B(0),Z(10));
    U_mux11: mux2 port map (A(11), A(19), B(0),Z(11));
    U_mux12: mux2 port map (A(12), A(20), B(0),Z(12));
    U_mux13: mux2 port map (A(13), A(21), B(0),Z(13));
    U_mux14: mux2 port map (A(14), A(22), B(0),Z(14));
    U_mux15: mux2 port map (A(15), A(23), B(0),Z(15));
    U_mux16: mux2 port map (A(16), A(24), B(0),Z(16));
    U_mux17: mux2 port map (A(17), A(25), B(0),Z(17));
    U_mux18: mux2 port map (A(18), A(26), B(0),Z(18));
    U_mux19: mux2 port map (A(19), A(27), B(0),Z(19));
    U_mux20: mux2 port map (A(20), A(28), B(0),Z(20));
    U_mux21: mux2 port map (A(21), A(29), B(0),Z(21));
    U_mux22: mux2 port map (A(22), A(30), B(0),Z(22));
    U_mux23: mux2 port map (A(23), A(31), B(0),Z(23));
    U_mux24: mux2 port map (A(24), '0', B(0),Z(24));
    U_mux25: mux2 port map (A(25), '0', B(0),Z(25));
    U_mux26: mux2 port map (A(26), '0', B(0),Z(26));
    U_mux27: mux2 port map (A(27), '0', B(0),Z(27));
    U_mux28: mux2 port map (A(28), '0', B(0),Z(28));
    U_mux29: mux2 port map (A(29), '0', B(0),Z(29));
    U_mux30: mux2 port map (A(30), '0', B(0),Z(30));
    U_mux31: mux2 port map (A(31), '0', B(0),Z(31));
<<<<<<< HEAD
    
=======

>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity shiftRight16 is
  port(A,B : in  reg32;
       Z   : out reg32);
end shiftRight16;

<<<<<<< HEAD
architecture estrut of shiftRight16 is 
  
  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;
  
 begin  
=======
architecture estrut of shiftRight16 is

  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;

 begin
>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
    U_mux0: mux2 port map (A(0), A(16), B(0),Z(0));
    U_mux1: mux2 port map (A(1), A(17), B(0),Z(1));
    U_mux2: mux2 port map (A(2), A(18), B(0),Z(2));
    U_mux3: mux2 port map (A(3), A(19), B(0),Z(3));
    U_mux4: mux2 port map (A(4), A(20), B(0),Z(4));
    U_mux5: mux2 port map (A(5), A(21), B(0),Z(5));
    U_mux6: mux2 port map (A(6), A(22), B(0),Z(6));
    U_mux7: mux2 port map (A(7), A(23), B(0),Z(7));
    U_mux8: mux2 port map (A(8), A(24), B(0),Z(8));
    U_mux9: mux2 port map (A(9), A(25), B(0),Z(9));
    U_mux10: mux2 port map (A(10), A(26), B(0),Z(10));
    U_mux11: mux2 port map (A(11), A(27), B(0),Z(11));
    U_mux12: mux2 port map (A(12), A(28), B(0),Z(12));
    U_mux13: mux2 port map (A(13), A(29), B(0),Z(13));
    U_mux14: mux2 port map (A(14), A(30), B(0),Z(14));
    U_mux15: mux2 port map (A(15), A(31), B(0),Z(15));
    U_mux16: mux2 port map (A(16), '0', B(0),Z(16));
    U_mux17: mux2 port map (A(17), '0', B(0),Z(17));
    U_mux18: mux2 port map (A(18), '0', B(0),Z(18));
    U_mux19: mux2 port map (A(19), '0', B(0),Z(19));
    U_mux20: mux2 port map (A(20), '0', B(0),Z(20));
    U_mux21: mux2 port map (A(21), '0', B(0),Z(21));
    U_mux22: mux2 port map (A(22), '0', B(0),Z(22));
    U_mux23: mux2 port map (A(23), '0', B(0),Z(23));
    U_mux24: mux2 port map (A(24), '0', B(0),Z(24));
    U_mux25: mux2 port map (A(25), '0', B(0),Z(25));
    U_mux26: mux2 port map (A(26), '0', B(0),Z(26));
    U_mux27: mux2 port map (A(27), '0', B(0),Z(27));
    U_mux28: mux2 port map (A(28), '0', B(0),Z(28));
    U_mux29: mux2 port map (A(29), '0', B(0),Z(29));
    U_mux30: mux2 port map (A(30), '0', B(0),Z(30));
    U_mux31: mux2 port map (A(31), '0', B(0),Z(31));
<<<<<<< HEAD
    
=======

>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity shiftRight32 is
  port(A,B : in  reg32;
       Z   : out reg32);
end shiftRight32;

<<<<<<< HEAD
architecture estrut of shiftRight32 is 
  
  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;
  
  component shiftRight1 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftRight1;
  
  component shiftRight2 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftRight2;
  
  component shiftRight4 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftRight4;
  
  component shiftRight8 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftRight8;
  
  component shiftRight16 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftRight16;
  
  signal J,K,L,M,N : reg32;
 begin  
=======
architecture estrut of shiftRight32 is

  component mux2 is
    port(A,B: in bit; S: in bit; Z: out bit);
  end component mux2;

  component shiftRight1 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftRight1;

  component shiftRight2 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftRight2;

  component shiftRight4 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftRight4;

  component shiftRight8 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftRight8;

  component shiftRight16 is
    port(A,B: in reg32; Z: out reg32);
  end component shiftRight16;

  signal J,K,L,M,N : reg32;
 begin
>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
    U_shift1: shiftRight1 port map (A,B,J);
    U_shift2: shiftRight2 port map (J,B,K);
    U_shift4: shiftRight4 port map (K,B,L);
    U_shift8: shiftRight8 port map (L,B,M);
    U_shift16: shiftRight16 port map (M,B,N);
<<<<<<< HEAD
    
=======

>>>>>>> 72a96ec75e7f18b217c7dcb2272ff74857244fdc
    Z <= N when B <= x"20" else (x"00000000");
end architecture estrut;

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- display: exibe inteiro na saida padrao do simulador
--          NAO ALTERE ESTE MODELO
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use std.textio.all;
use work.p_wires.all;

entity display is
port (rst,clk : in bit;
  enable  : in bit;
  data    : in reg32);
end display;

architecture functional of display is
file output : text open write_mode is "STD_OUTPUT";
begin  -- functional

U_WRITE_OUT: process(clk)
variable msg : line;
begin
if falling_edge(clk) and enable = '1' then
write ( msg, string'(BV32HEX(data)) );
writeline( output, msg );
end if;
end process U_WRITE_OUT;

end functional;
-- ++ display ++++++++++++++++++++++++++++++++++++++++++++++++++++++++



-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- memoria RAM, com capacidade de 64K palavras de 32 bits
--          NAO ALTERE ESTE MODELO
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee; use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.p_wires.all;

entity RAM is
port (rst, clk : in  bit;
        sel      : in  bit;          -- ativo em 1
        wr       : in  bit;          -- ativo em 1
        ender    : in  reg16;
        data_inp : in  reg32;
        data_out : out reg32);

constant DATA_MEM_SZ : natural := 2**16;
constant DATA_ADDRS_BITS : natural := log2_ceil(DATA_MEM_SZ);

end RAM;

architecture rtl of RAM is

subtype t_address is unsigned((DATA_ADDRS_BITS - 1) downto 0);

subtype word is bit_vector(31 downto 0);
type storage_array is
array (natural range 0 to (DATA_MEM_SZ - 1)) of word;
signal storage : storage_array;
begin

accessRAM: process(rst, clk, sel, wr, ender, data_inp)
variable u_addr : t_address;
variable index, latched : natural;

variable d : reg32 := (others => '0');
variable val, i : integer;

begin

    if (rst = '0') and (sel = '1') then -- normal operation

    index := BV2INT16(ender);

    if  (wr = '1') and rising_edge(clk) then

    assert (index >= 0) and (index < DATA_MEM_SZ)
    report "ramWR index out of bounds: " & natural'image(index)
    severity failure;

    storage(index) <= data_inp;

    assert TRUE report "ramWR["& natural'image(index) &"] "
          & BV32HEX(data_inp); -- DEBUG

          else

          assert (index >= 0) and (index < DATA_MEM_SZ)
          report "ramRD index out of bounds: " & natural'image(index)
          severity failure;

          d := storage(index);

          assert TRUE report "ramRD["& natural'image(index) &"] "
          & BV32HEX(d);  -- DEBUG

      end if; -- normal operation

      data_out <= d;

      else

      data_out <= (others=>'0');

    end if; -- is reset?

  end process accessRAM; -- ---------------------------------------------

  end rtl;
-- -----------------------------------------------------------------------



-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- banco de registradores
--          NAO ALTERE ESTE MODELO
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity R is
port (clk         : in  bit;
        wr_en       : in  bit;          -- ativo em 1
        r_a,r_b,r_c : in  reg4;
        A,B         : out reg32;
        C           : in  reg32);
end R;

architecture rtl of R is
type reg_file is array(0 to 15) of reg32;
signal reg_file_A : reg_file;
signal reg_file_B : reg_file;
signal int_ra, int_rb, int_rc : integer range 0 to 15;
begin

int_ra <= BV2INT4(r_a);
int_rb <= BV2INT4(r_b);
int_rc <= BV2INT4(r_c);

A <= reg_file_A( int_ra ) when r_a /= b"0000" else
       x"00000000";                        -- reg0 always zero
       B <= reg_file_B( int_rb ) when r_b /= b"0000" else
       x"00000000";

       WRITE_REG_BANKS: process(clk)
       begin
       if rising_edge(clk) then
       if wr_en = '1' and r_c /= b"0000" then
       reg_file_A( int_rc ) <= C;
       reg_file_B( int_rc ) <= C;
       end if;
       end if;
       end process WRITE_REG_BANKS;
       end rtl;
-- -----------------------------------------------------------------------
