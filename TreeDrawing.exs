# LIP - Quentão 1 - Desenhando árvores
# Dupla:
# Francisco Jean da Silva de Sousa  - Matrícula: 541790
# Victoria de Castro Moura          - Matrícula: 541801


# Definição da struct para os nós da árvore
defmodule Tree do
  defstruct key: nil, val: nil, left: nil, right: nil

  # Nó tipo folha
  def leaf do
    :leaf
  end

  # Função que cria um novo nó Tree
  def tree(key, val, left \\ leaf(), right \\ leaf()) do
    %Tree{key: key, val: val, left: left, right: right}
  end
end

# Módulo que define as funções de percurso na árvore
defmodule DepthFirst do

  # Distância mínima entre os nós que estão no mesmo nível da Tree
  @scale 30
  
  defstruct tree: nil, rootX: nil, rightLim: nil

  def scale do
    @scale
  end

  # Tripla que define a forma do retorno das variaves não
  # ligadas que seriam parâmetro das funções
  def tripla(tree, rx, rlim) do
    %DepthFirst{tree: tree, rootX: rx, rightLim: rlim}
  end 

  # Função que adiciona os campos x e y em um nó da árvore
  # e retorna um novo nó com esses campos
  def addXY(tree, x, y) do
    Map.merge(tree, %{x: x, y: y})
  end

  # Função que percorre a árvore e calcula os valores de x e y para cada
  # nó. Foi alterada um pouco da lógica da função em relação da forma que
  # o livro texto implementa, de forma que no próprio percurso os campos x e y são 
  # adicionados nos nós da árvore
  def depthFirst(headKey, tree, level, leftLim) do

    # Verifica qual tipo de nó será processado
    case tree do
      # Nó com dois filhos folha
      %Tree{left: :leaf, right: :leaf} ->
        x = leftLim
        y = @scale * level

        if headKey == tree.key do
          addXY(tree, x, y)
        else
          tripla(addXY(tree, x, y), x, x)
        end

      # Nó com apenas o filho direito folha
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

      # Nó com apenas o filho esquerdo folha
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

      # Nó com os dois filhos não folha
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

# Módulo que imprime a árvore com os campos x e y
defmodule PrintTree do
  # Percurso na árvore sintática para a impressão das expressões de cada nó
  def print(node, level) do
    case node do
    # Printa o nó que possui os dois filhos folha
    %Tree{left: :leaf, right: :leaf} ->
      space = String.duplicate(" ", level * 4)
      "%Tree{\n#{space}key: #{node.key}\n#{space}value: #{node.val}\n#{space}x: #{node.x}\n#{space}y: #{node.y}\n#{space}left: :leaf\n#{space}right: :leaf}"

    # Printa o nó que possui o filho direito folha e chama a função
    # para o filho esquerdo
    %Tree{left: %Tree{}, right: :leaf} ->
      space = String.duplicate(" ", level * 4)
      "%Tree{\n#{space}key: #{node.key}\n#{space}value: #{node.val}\n#{space}x: #{node.x}\n#{space}y: #{node.y}\n#{space}left: #{print(node.left, level + 1)}\n#{space}right: :leaf}"

    # Printa o nó que possui o filho esquerdo folha e chama a função
    # para o filho direito
    %Tree{left: :leaf, right: %Tree{}} ->
      space = String.duplicate(" ", level * 4)
      "%Tree{\n#{space}key: #{node.key}\n#{space}value: #{node.val}\n#{space}x: #{node.x}\n#{space}y: #{node.y}\n#{space}left: :leaf\n#{space}right: #{print(node.right, level + 1)}}"

    # Printa o nó e chama a função para os seus dois filhos
    %Tree{left: %Tree{}, right: %Tree{}} ->
      space = String.duplicate(" ", level * 4)
      "%Tree{\n#{space}key: #{node.key}\n#{space}value: #{node.val}\n#{space}x: #{node.x}\n#{space}y: #{node.y}\n#{space}left: #{print(node.left, level + 1)}\n#{space}right: #{print(node.right, level + 1)}}"

    _ ->
      raise "Entrada Inválida"

    end
  end
end


# Definição do módulo principal
defmodule Main do
  def main do
    # Árvore de exemplo da figura 3.17 do livro texto
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

    # Chamada da função para o calculo das coordenadas
    arvore = DepthFirst.depthFirst(tree.key, tree, 1, DepthFirst.scale())

    # Impressão da estrutura da árvore modificada
    IO.puts("\nArvore modificada:\n")
    IO.puts(PrintTree.print(arvore, 1))
  end
end

Main.main()

