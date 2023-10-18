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

tree =
  Tree.tree(:a, 111,
    Tree.tree(:b, 55,
      Tree.tree(:x, 100,
        Tree.tree(:z, 56),
        Tree.tree(:w, 23)
      ),
      Tree.tree(:y, 105, :leaf,
        Tree.tree(:r, 77)
      )
    ),
    Tree.tree(:c, 123,
      Tree.tree(:d, 119,
        Tree.tree(:g, 44),
        Tree.tree(:h, 50,
          Tree.tree(:i, 5),
          Tree.tree(:j, 6)
        )
      ),
      Tree.tree(:e, 133)
    )
  )


arvore = DepthFirst.depthFirst(tree.key, tree, 1, DepthFirst.scale())

IO.puts("\nArvore modificada:\n")
IO.inspect(arvore)
