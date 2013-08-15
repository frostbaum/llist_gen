module llist_c
  implicit none
  private
  public :: llist, list_data
  public :: list_add, list_next, list_rewind, list_clear, list_get, list_status, list_merge

  integer, dimension(:), allocatable :: list_data

  type item
    integer, dimension(:), pointer :: data => null()
    type(item), pointer :: next => null()
  end type

  type llist
    private
    type(item) :: head
    type(item), pointer :: cpos => null()
  end type

contains

  subroutine list_add(this,data)
    type(llist) :: this
    integer, dimension(:) :: data
    type(item), pointer :: next => null()

    allocate(next)
    allocate(next%data(size(data)))
    next%data = data

    next%next => this%head%next
    this%head%next => next
    this%cpos => next
  end subroutine

  subroutine list_next(this,status)
    type(llist) :: this
    logical :: status

    if (associated(this%cpos%next)) then
      status = .true.
      this%cpos => this%cpos%next
    else
      status = .false.
    end if
    
  end subroutine
  
  subroutine list_rewind(this)
    type(llist) :: this
    
    this%cpos => this%head%next
  end subroutine

  subroutine list_clear(this)
    type(llist) :: this
    type(item), pointer :: current => null()
    type(item), pointer :: next => null()

    current => this%head%next
    nullify(this%head%next)
    nullify(this%cpos)

    do while (associated(current))
      next => current%next
      deallocate(current%data)
      nullify(current%data)
      deallocate(current)
      current => next
    end do
  end subroutine

  function list_get(this) result(data)
    type(llist) :: this
    integer, dimension(:), pointer :: data

    data => this%cpos%data
  end function
  
  function list_status(this) result(status)
    type(llist) :: this
    logical :: status
    
    status = associated(this%cpos)
  end function
  
  subroutine list_merge(list1,list2)
    type(llist), intent(inout) :: list1
    type(llist), intent(in) :: list2
    logical :: status
    
    status = list_status(list1)
    
    do while (status)
      call list_next(list1,status)
    end do
    
    list1%cpos%next => list2%head%next
  end subroutine
end module
