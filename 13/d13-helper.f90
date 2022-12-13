module d13_helper
  implicit none

  private
  public :: build_tree, compare_trees, node_types, tree_node, walk_tree


  type :: node_types_enum
    integer :: unknown = 0
    integer :: intnode = 1
    integer :: listnode = 2
  end type node_types_enum

  type(node_types_enum), parameter :: node_types = node_types_enum()


  type :: tree_node
    integer :: nodetype = node_types%unknown
    integer :: intval = 0
    type(tree_node), allocatable :: child
    type(tree_node), allocatable :: next
  end type tree_node


contains


  subroutine build_tree(line, node)
    character(*), intent(in) :: line
    type(tree_node), allocatable, intent(out) :: node

    type(tree_node), allocatable :: head
    integer :: pos
    logical :: finished

    finished = .false.
    pos = 1
    allocate(head)
    call build_tree_helper_(line, pos, head)
    call move_alloc(head%child, node)

  end subroutine build_tree


  recursive subroutine build_tree_helper_(line, pos, parent)
    character(*), intent(in) :: line
    integer, intent(inout) :: pos
    type(tree_node), intent(inout), target :: parent

    integer :: lastseppos
    integer, allocatable :: intbuff(:)
    character :: curchar
    type(tree_node), pointer :: curnode

    curnode => null()
    lastseppos = pos - 1
    do while (pos <= len(line))
      curchar = line(pos:pos)
      if (curchar == "[") then
        if (.not. associated(curnode)) then
          allocate(parent%child)
          curnode => parent%child
        else
          allocate(curnode%next)
          curnode => curnode%next
        end if
        curnode%nodetype = node_types%listnode
        pos = pos + 1
        call build_tree_helper_(line, pos, curnode)
        lastseppos = pos - 1
      elseif (curchar == "]" .or. curchar == ",") then
        if (lastseppos < pos - 1) then
          if (.not. associated(curnode)) then
            allocate(parent%child)
            curnode => parent%child
          else
            allocate(curnode%next)
            curnode => curnode%next
          end if
          curnode%nodetype = node_types%intnode
          read(line(lastseppos + 1 : pos - 1), *) curnode%intval
        end if
        lastseppos = pos
        pos = pos + 1
        if (curchar == "]") exit
      else
        pos = pos + 1
      end if
    end do

  end subroutine build_tree_helper_


  recursive subroutine walk_tree(node)
    type(tree_node), target, intent(in) :: node

    type(tree_node), pointer :: curnode

    curnode => node
    do while (associated(curnode))
      if (curnode%nodetype == node_types%intnode) then
        print *, "INTVAL: ", curnode%intval
      else
        print *, "OPEN LIST", curnode%nodetype
        if (allocated(curnode%child)) call walk_tree(curnode%child)
        print *, "CLOSE LIST"
      end if
      curnode => tree_node_ptr_(curnode%next)
    end do

  end subroutine walk_tree


  recursive function compare_trees(lhs, rhs) result(compval)
    type(tree_node), target, intent(in) :: lhs, rhs
    integer :: compval

    type(tree_node), allocatable :: buffer
    type(tree_node), pointer :: curlhs, currhs

    if (lhs%nodetype == node_types%intnode .and. rhs%nodetype == node_types%intnode) then
      compval = lhs%intval - rhs%intval
      if (compval /= 0) compval = compval / abs(compval)

    else if (lhs%nodetype == node_types%intnode .and. rhs%nodetype == node_types%listnode) then
      buffer = tree_node(nodetype=node_types%listnode,&
          & child=tree_node(nodetype=node_types%intnode, intval=lhs%intval))
      compval = compare_trees(buffer, rhs)

    else if (lhs%nodetype == node_types%listnode .and. rhs%nodetype == node_types%intnode) then
      buffer = tree_node(nodetype=node_types%listnode,&
          & child=tree_node(nodetype=node_types%intnode, intval=rhs%intval))
      compval = compare_trees(lhs, buffer)

    else if (lhs%nodetype == node_types%listnode .and. rhs%nodetype == node_types%listnode) then
      curlhs => tree_node_ptr_(lhs%child)
      currhs => tree_node_ptr_(rhs%child)
      do while (associated(curlhs) .and. associated(currhs))
        compval = compare_trees(curlhs, currhs)
        if (compval /= 0) return
        curlhs => tree_node_ptr_(curlhs%next)
        currhs => tree_node_ptr_(currhs%next)
      end do
      if (associated(curlhs) .and. .not. associated(currhs)) then
        compval = 1
      else if (.not. associated(curlhs) .and. associated(currhs)) then
        compval = -1
      else
        compval = 0
      end if
    end if

  end function compare_trees


  function tree_node_ptr_(node) result(ptr)
    type(tree_node), pointer, optional, intent(in) :: node
    type(tree_node), pointer :: ptr

    if (present(node)) then
      ptr => node
    else
      ptr => null()
    end if

  end function tree_node_ptr_

end module d13_helper
