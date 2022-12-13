program d13_1
  use d13_helper, only : build_tree, compare_trees, tree_node
  implicit none

  integer, parameter :: max_line_len = 1024

  integer :: fd, iostat, icycle, indsum
  character(max_line_len) :: line
  type(tree_node), allocatable :: tree1, tree2

  indsum = 0
  icycle = 0
  open(newunit=fd, file="input.dat")
  do
    icycle = icycle + 1
    read(fd, "(a)", iostat=iostat) line
    if (iostat /= 0) exit
    call build_tree(trim(line), tree1)
    read(fd, "(a)", iostat=iostat) line
    if (iostat /= 0) exit
    call build_tree(trim(line), tree2)
    if (compare_trees(tree1, tree2) <= 0) indsum = indsum + icycle
    read(fd, *, iostat=iostat)
    if (iostat /= 0) exit
  end do
  close(fd)

  print "(a, i0)", "Index sum of packages in right order: ", indsum

end program d13_1
