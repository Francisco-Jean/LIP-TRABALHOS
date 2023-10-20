# [cabeça | cauda] = s1
# case  
# peg o retorno do primeiro if 
# tupla= {true false(is atom), atom, resto da lista}
# head = elem[tupla,0]
# stat retorna o nó + resto da lista 
# prog monta nó principal
# stat monta sub nós


defmodule Parser do

  defmodule NodeInit do
    defstruct name: nil, left: nil, right: nil

    def new(name, left, right) do
      %NodeInit{name: name, left: left, right: right}
    end
  end

  defmodule NodeState do
    defstruct state: nil, operator: nil, left: nil, right: nil

    def new(state, operator, left, right) do
      %NodeState{state: state, operator: operator, left: left, right: right}
    end
  end


  defmodule NodeUnario do
    defstruct state: nil, id: nil

    def new(state, id) do
      %NodeUnario{state: state, id: id}
    end
  end


  defmodule NodeBinario do
    defstruct operator: nil, left: nil, right: nil

    def new(operator, left, right) do
      %NodeBinario{operator: operator, left: left, right: right}
    end
  end


  def prog(s1) do 
    [head1 | s2] = s1
    if head1 == :program do
      result=id(s2)
      if elem(result,0) or not isIdent(elem(result,1)) do
        y = elem(result,1)
        s3 = elem(result,2)
        [head2 | s4]=s3
        if head2 == ';' do
          {z,s5}=stat(s4)
          [head3 | sn] = s5
          if head3 == 'end' do
            {NodeInit.new(:program, y, z), sn}
          end
        end
      end
    end
  end 

  def stat(s1) do
    [t | s2]=s1
    case t do
    :if -> 
      {c,s3} = comp(s2)
      [head4 | s4]=s3
      if head4 == :then do
        {x1,s5}=stat(s4)
        [head5 | s6]=s5
        if head5 == :else do
          {x2,sn}=stat(s6)
          {NodeState.new(:if, c, x1, x2), sn}
        else
          {NodeBinario.new(:if, c, x1), s5}
        end
      end

    :while ->
      {c, s3} = comp(s2)
      [head6|s4] = s3
      if head6 == :do do
        {x,sn} = stat(s4)
        {NodeBinario.new(:while, c, x), sn}
      end

    :read ->
      {resul,i,sn} = id(s2)
      if resul do
        {NodeUnario.new(:read, i), sn}
      end

    :write ->
      {e,sn} = expr(s2)
      {NodeUnario.new(:read, e), sn}

    _ ->
      if isIdent(t) do
        [head7|s3] = s2
        if head7 == ':=' do
          {e, sn} = expr(s3)
          {NodeBinario.new(':=', t, e), sn}
        else
          raise "token não identificado"
        end
      end
    end
  end

  def sequence(nonTerm, sep, s1) do
    {x1, s2} = nonTerm.(s1)
    [t|s3] = s2
    if sep.(t) do
      {x2, sn} = sequence(nonTerm, sep, s3)
      {NodeBinario.new(t, x1, x2), sn}
    else
      {x1, s2}
    end
  end

  def comp(s1) do
    sequence(&expr/1, &cop/1, s1)
  end

  def expr(s1) do
    sequence(&term/1, &eop/1, s1)
  end

  def term(s1) do
    sequence(&fact/1, &top/1, s1)
  end

  def cop(y) do
    y == '<' or y == '>' or y == '=<' or
    y == '>' or y == '==' or y == '!='
  end

  def eop(y) do
    y == '+' or y == '-'
  end

  def top(y) do
    y == '*' or y == '/'
  end


  def fact(s1) do
    [t|s2] = s1
    if is_integer(t) or isIdent(t) do
      {t, s2}
    else
      [a|s2] = s1
      if a == '(' do
        {e, s3} = expr(s2)
        [a|sn] = s3
        if a == ')' do
          {e, sn}
        else
          raise "entrada incompatível."
        end
      else
        raise "entrada incompatível."
      end
    end
  end


  def id(s1) do
    case s1 do
    nil -> false
    _ -> 
      [x|sn] = s1
      if isIdent(x) do 
        {true, x, sn}
      else
        {false, x, sn}
      end
    end
  end

  def isIdent(x) do
    e_atom(x)
  end

  def e_atom(id) do
    Enum.member?([';', :if, :while, :read, :write, :do, ':='], id) or is_atom(id)
  end
end


programa = [:program, 'progName', ';', :if, :b, '<', 3, :then, :x, ':=', 1, '+', 54, :else, :x, ':=', 1, 'end']
{syntatic, sn} = Parser.prog(programa)
IO.inspect(syntatic)
