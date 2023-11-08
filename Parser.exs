# LIP - Quentão 2 - Parser em elixir
# Dupla:
# Francisco Jean da Silva de Sousa  - Matrícula: 541790
# Victoria de Castro Moura          - Matrícula: 541801

defmodule Parser do

  # Definição do nó da árvore sintática do programa principal
  defmodule NodeProg do
    defstruct prog: nil, progName: nil, state: nil

    def new(prog, progName, state) do
      %NodeProg{prog: prog, progName: progName, state: state}
    end
  end

  # Definição do nó da árvore sintática do token state (if)
  defmodule NodeState do
    defstruct state: nil, operator: nil, left: nil, right: nil

    def new(state, operator, left, right) do
      %NodeState{state: state, operator: operator, left: left, right: right}
    end
  end

  # Definição do nó da árvore sintática do token state com apenas um filho (Read e Write)
  defmodule NodeUnario do
    defstruct state: nil, id: nil

    def new(state, id) do
      %NodeUnario{state: state, id: id}
    end
  end

  # Definição do nó da árvore sintática do token state com apenas dois filhos (Read e Write)
  defmodule NodeBinario do
    defstruct operator: nil, left: nil, right: nil

    def new(operator, left, right) do
      %NodeBinario{operator: operator, left: left, right: right}
    end
  end

def prog(s1) do 
  [head1 | s2] = s1

  if head1 == :program do
    result = id(s2)

    if elem(result, 0) or not isIdent(elem(result, 1)) do
      y = elem(result, 1)
      s3 = elem(result, 2)
      [head2 | s4] = s3

      if head2 == ';' do
        {z, s5} = stat(s4)
        [head3 | sn] = s5

        if head3 == 'end' do

        # Forma de driblar a falta de variáveis não ligadas.
        # O retorno das funções que utilizam variaveis não 
        # ligadas como parâmetro é uma tupla (ou tripla,
        # dependendo da função) que retorna o que normalmente
        # A função retornaria com o acrescimo dos valores
        # das variaveis que seriam não ligadas.
        # Dessa forma, esses valores são recebidos no retorno
        # da função e atribuido para as respectivas variaveis:
      
          {NodeProg.new(:program, y, z), sn}
        else
          {:error, "SyntaxError: 'end' era esperado mas não foi encontrado"}
        end
      else
        {:error, "SyntaxError: erro ao analisar a instrução após ';'"}
      end
    else
      {:error, "SyntaxError: ';' era esperado mas não foi encontrado."}
    end
  else
    {:error, "SyntaxError: 'program' era esperado mas não foi encontrado."}
  end
end


  # Função que analisa os tokens de entrada e verifica qual tipo 
  # de átomo deve ser analisado
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
      else
        raise "SyntaxError: 'then' era esperado mas não foi encontrado."
      end

    :while ->
      {c, s3} = comp(s2)
      [head6|s4] = s3
      if head6 == :do do
        {x,sn} = stat(s4)
        {NodeBinario.new(:while, c, x), sn}
      else
      raise "SyntaxError: 'do' era esperado mas não foi encontrado."
      end

    :read ->
      {resul,i,sn} = id(s2)
      if resul do
        {NodeUnario.new(:read, i), sn}
      else
        raise "SyntaxError: erro ao analisar 'read'."
      end

    :write ->
      {e,sn} = expr(s2)
      {NodeUnario.new(:read, e), sn}

    _ ->
      if isIdent(t) do
        [head7|s3] = s2
        if head7 == ':=' do
          {e, sn} = expr(s3)
          {NodeBinario.new(:assign, t, e), sn}
        else
          raise "TokenError: token #{t} não identificado."
        end
      end
    end
  end

  # Função genérica que analisa as sequências de instruções, 
  # sequências de comparações, sequência de espressões e sequêndia
  # de termos
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
          raise "SyntaxError: ) era esperado mas não foi encontrado."
        end
      else
        raise "SyntaxError: ( era esperado mas não foi encontrado."
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

# Módulo que implementa uma função para imprimir a saida semelhante ao código em Oz
defmodule PrintTree do
  # Percurso na árvore sintática para a impressão das expressões de cada nó
  def print(node) do
    case node do
    # Impressão do nó principal e chamada para os nós internos
    %Parser.NodeProg{progName: n, state: s} ->
      ":program (#{n} (#{print(s)})"

    # Impressão do nó do tipo state chamada para os nós internos
    %Parser.NodeState{state: state, operator: operator, left: left, right: right} ->
      "#{state}(#{print(operator)}(#{print(left)} #{print(right)}))"

    # Impressão do nó de operação binária e chamada para as expressões filhas
    %Parser.NodeBinario{operator: operator, left: left, right: right} ->
      l = print(left)
      r = print(right)
      "#{operator} (#{l} #{r})"

    # Impressão do nó de operação de leitura e escrita
    %Parser.NodeUnario{state: state, id: id} ->
      "#{state} (#{id})"
      
    _ ->
      "#{node}"
      
    end
  end
end


defmodule Main do
  def main do
    # Programa de entrada semelhante ao do livro texto
    programa = [:program, 'progJeanVictoria', ';', :while, :a, '+', 3, '<', :b, :do, :b, ':=', :b, '+', 1, 'end']
    {syntatic, _sn} = Parser.prog(programa)
    IO.puts("1. Nós da árvore:\n")
    IO.inspect(syntatic)
    IO.puts("\n2. Árvore Sintatica:\n")
    IO.puts(PrintTree.print(syntatic) <> "\n")
  end
end

Main.main()
