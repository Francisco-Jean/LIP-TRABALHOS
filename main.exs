defmodule Tree do
  defstruct key: nil, val: nil, left: nil, right: nil
  
  def leaf do
    :leaf
  end
  
  def tree(key, val, left \\ leaf(), right \\ leaf()) do
    %Tree{key: key, val: val, left: left, right: right}
  end
end

defmodule DepthFirst do

  @scale 30
  
  defstruct tree: nil, rootX: nil, rightLim: nil

  def scale do
    @scale
  end
  
  def tripla(tree, rx, rlim) do
    %DepthFirst{tree: tree, rootX: rx, rightLim: rlim}
  end 
  
  def addXY(tree, x, y) do
    Map.merge(tree, %{x: x, y: y})
  end

  def depthFirst(headKey, tree, level, leftLim) do
    
    case tree do
      %Tree{left: :leaf, right: :leaf} ->
        x = leftLim
        y = @scale * level

        if headKey == tree.key do
          addXY(tree, x, y)
        else
          tripla(addXY(tree, x, y), x, x)
        end
        
      %Tree{left: %Tree{}, right: :leaf} ->
        tripla1 = depthFirst(headKey, tree.left, level + 1, leftLim)
        x = tripla1.rootX
        y = @scale * level
        left = tripla1.tree

        if headKey == tree.key do
          Map.merge(addXY(tree, x, y), %{left: left})
        else
          tripla(Map.merge(addXY(tree, x, y), %{left: left}), tripla1.rootX, tripla1.rightLim)
        end
        
      %Tree{left: :leaf, right: %Tree{}} ->
        tripla1 = depthFirst(headKey, tree.right, level + 1, leftLim)
        x = tripla1.rootX
        y = @scale * level
        right = tripla1.tree
        
        if headKey == tree.key do
          Map.merge(addXY(tree, x, y), %{right: right})
        else
          tripla(Map.merge(addXY(tree, x, y), %{right: right}), tripla1.rootX, tripla1.rightLim)
        end
        
      %Tree{left: %Tree{}, right: %Tree{}} ->
        y = @scale * level
        tripla1 = depthFirst(headKey, tree.left, level + 1, leftLim)
        rLeftLim = tripla1.rightLim + @scale
        tripla2 = depthFirst(headKey, tree.right, level + 1, rLeftLim)
        x = (tripla1.rootX + tripla2.rootX) / 2
        left = tripla1.tree
        right = tripla2.tree
        if headKey == tree.key do
          Map.merge(addXY(tree, x, y), %{left: left, right: right})
        else
          tripla(Map.merge(addXY(tree, x, y), %{left: left, right: right}), x, tripla2.rightLim)
        end
    end
  end
end


# Modulo extra que implementa uma função que gera uma árvore com n nós com chaves e valores dos nós no intervalo de 1 a n
defmodule TreeGenerator do
  import Tree

  def generate_tree(n) when is_integer(n) and n >= 1 do
    generate_tree(1, n)
  end

  defp generate_tree(n, n) do
    tree(n, n)
  end

  defp generate_tree(i, n) when i <= n do
    left_key = i * 2
    right_key = i * 2 + 1

    left_tree =
      if left_key <= n do
        generate_tree(left_key, n)
      else
        :leaf
      end

    right_tree =
      if right_key <= n do
        generate_tree(right_key, n)
      else
        :leaf
      end

    tree(i, i, left_tree, right_tree)
  end
end


tree = TreeGenerator.generate_tree(8)
IO.puts("Arvore original: ")
IO.inspect(tree)
arvore = DepthFirst.depthFirst(tree.key, tree, 1, DepthFirst.scale())
IO.puts("\nArvore modificada: ")
IO.inspect(arvore)




