proc {DepthFirst Tree Level LeftLim ?RootX ?RightLim}
  case Tree
  of tree(x:X y:Y left:leaf right:leaf ...) then
    X=RootX=RightLim=LeftLim
    Y=Scale*Level
  [] tree(x:X y:Y left:L right:leaf ...) then
    X=RootX
    Y=Scale*Level
    {DepthFirst L Level+1 LeftLim RootX RightLim}
  [] tree(x:X y:Y left:leaf right:R ...) then
    X=RootX
    Y=Scale*Level
    {DepthFirst R Level+1 LeftLim RootX RightLim}
  [] tree(x:X y:Y left:L right:R ...) then
    LRootX LRightLim RRootX RLeftLim
    in
      Y=Scale*Level
      {DepthFirst L Level+1 LeftLim LRootX LRightLim}
      RLeftLim=LRightLim+Scale
      {DepthFirst R Level+1 RLeftLim RRootX RightLim}
      X=RootX=(LRootX+RRootX) div 2
  end
end