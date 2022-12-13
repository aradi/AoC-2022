program d13_2
  use d13_helper, only : build_tree, compare_trees, node_types, tree_node
  implicit none

  integer, parameter :: max_line_len = 1024

  integer :: fd, iostat, irecord, idivpack
  character(max_line_len) :: line
  type(tree_node), allocatable :: tree
  type(tree_node) :: divpackages(2)
  integer :: divpackinds(2)

  divpackages(:) = [&
      & tree_node(nodetype=node_types%listnode,&
      & child=tree_node(nodetype=node_types%listnode,&
      & child=tree_node(nodetype=node_types%intnode, intval=2))),&
      &&
      & tree_node(nodetype=node_types%listnode,&
      & child=tree_node(nodetype=node_types%listnode,&
      & child=tree_node(nodetype=node_types%intnode, intval=6)))&
      & ]
  divpackinds(:) = [1, 2]

  open(newunit=fd, file="input.dat")
  linereader: do
    do irecord = 1, 2
      read(fd, "(a)", iostat=iostat) line
      if (iostat /= 0) exit linereader
      call build_tree(trim(line), tree)
      do idivpack = 1, size(divpackages)
        if (compare_trees(tree, divpackages(idivpack)) == -1) then
          divpackinds(idivpack) = divpackinds(idivpack) + 1
        end if
      end do
    end do
    read(fd, *, iostat=iostat)
    if (iostat /= 0) exit
  end do linereader
  close(fd)

  print "(a, i0)", "Encoder key for distress signal: ", product(divpackinds)

end program d13_2
