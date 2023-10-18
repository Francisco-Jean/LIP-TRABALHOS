defmodule SyntaticTree do
  defstruct operator: nil, left: nil, right: nil

  defstruct state: nil, left: nil, right:nil, op:nil
  
  def leaf do
    :leaf
  end
  
  def tree(key, left \\ leaf(), right \\ leaf()) do
    %Tree{key: key, val: val, left: left, right: right}
  end

  def retNode()
end
# [cabeça | cauda] = s1
# case  
# peg o retorno do primeiro if 
# tupla= {true false(is atom), atom, resto da lista}
# head = elem[tupla,0]
# stat retorna o nó + resto da lista 
# prog monta nó principal
# stat monta sub nós
defmodule Parser do
  def prog(s1) do 
    [head1 | s2] = s1
    if head1 == :program do
      result=id(s2)
      if elem(result,0)== true do
        y=elem(result,1)
        s3=elem(result,2)
        [head2 | s4]=s3
        if head2 == ';' do        
          {z,s5}=stat(s4)
          [head3 | sn] = s5
          if head3 == 'end' do
            {SyntaticTree.tree(:program, y, z), sn}
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
            
        end
        
        
    end
    
  end
end

  def sequence(nonTerm, sep, s1) do
    {x1, s2} = nonTerm.(s1)
    [t|s3] = s2
    if sep.(t) do
      {x2, sn} = sequence(nonTerm, sep, s3)
      {SyntaticTree.tree(t, x1, x2), sn}
    else
      {x1, s2}
    end
  end
  
  def comp(s1) do
    sequence(expr cop s1)
  end

  def expr(s1) do
    sequence(term eop s1)
  end

  def term(s1) do
    sequence(fact top s1)
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
          SyntaticTree.tree(:TokenError, a, 'entrada incompatível.')
        end
      else
        SyntaticTree.tree(:TokenError, a, 'entrada incompatível.')
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
    is_atom(x)
  end