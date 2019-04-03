!------------------------------------------------------------------------------
!   KAUST/ESEP, Climate Modeling
!------------------------------------------------------------------------------
!
! MODULE:  FieldWPSList_mod
!
! AUTHOR:
! Suleiman Mostamandi, KAUST ESEP/PSE
! This code is modified version of Eric Kemp, NASA code
!
! DESCRIPTION:
! Defines data type and methods for creating linked list of FieldWPS data
! types.
!
! REVISION HISTORY:
! 17 Jan 2017 - Initial version
!
!------------------------------------------------------------------------------

module FieldWPSList_mod

   ! Import modules
   use FieldWPS_mod

   ! Defaults
   implicit none
   private

   ! Private data type
   type FieldWPSNode
      type(FieldWPS), pointer :: field
      type(FieldWPSNode), pointer :: next
      type(FieldWPSNode), pointer :: previous
   end type FieldWPSNode

   ! Public data type
   public :: FieldWPSList
   type FieldWPSList
      type(FieldWPSNode), pointer :: head
      type(FieldWPSNode), pointer :: tail
   end type FieldWPSList

   ! Public methods
   public :: createFieldWPSList
   public :: appendFieldWPSList
   public :: findFieldWPSList
   public :: destroyFieldWPSList
   public :: getXlvlsFieldWPSList

contains

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:
   ! createFieldWPSList
   !
   ! DESCRIPTION:
   ! Public "constructor method" for FieldWPSList data type.  List is
   ! empty; entries must be added separately.
   !
   !---------------------------------------------------------------------------

   function createFieldWPSList() result (this)

      ! Result
      type(FieldWPSList) :: this

      ! Initialize head node of linked list.
      nullify(this%head)
      nullify(this%tail)

      return
   end function createFieldWPSList

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:
   ! assignFieldWPSList
   !
   ! DESCRIPTION:
   ! Public method for adding entry to FieldWPSList data type.
   !
   !---------------------------------------------------------------------------

   subroutine appendFieldWPSList(this,field)

      ! Arguments
      type(FieldWPSList), intent(inout) :: this
      type(FieldWPS), intent(in) :: field

      ! Local variables
      type(FieldWPSNode), pointer :: newNode

      if (.not. associated(this%head)) then

         ! First case:  List is empty.  So allocate memory for first node and
         ! point to field.  Make sure we copy the values of field into the
         ! newly allocated memory of the pointer.

         allocate(newNode)
         allocate(newNode%field)
         newNode%field=copyFieldWPS(field)
         nullify(newNode%next)
         nullify(newNode%previous)

         this%head=>newNode
         this%tail=>newNode

      else

         ! Second case:  List is not empty.  So allocate memory for new node,
         ! copy the values of field into the node, and update pointers between
         ! nodes.
         allocate(newNode)
         allocate(newNode%field)
         newNode%field=copyFieldWPS(field)
         nullify(newNode%next)
         newNode%previous=>this%tail

         this%tail%next=>newNode

         this%tail=>newNode

      end if

      return
   end subroutine appendFieldWPSList

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:
   ! findFieldWPSList
   !
   ! DESCRIPTION:
   ! Public method for FieldWPSList to find an inserted FieldWPS member
   ! according to name and level.
   !
   !---------------------------------------------------------------------------

   function findFieldWPSList(this,fieldName,fieldXLvl) result (field)

      ! Arguments
      type(FieldWPSList), intent(in) :: this
      character(len=*),intent(in) :: fieldName
      real,intent(in) :: fieldXLvl

      ! Return variable
      type(FieldWPS) :: field
      type(FieldWPSNode),pointer :: node
      logical :: found

      found=.false.
      node=>this%head
      do
         ! First case:  We found the field.
         if (trim(node%field%field) .eq. trim(fieldName) .and. &
             node%field%xlvl .eq. fieldXLvl) then
            field=copyFieldWPS(node%field)
            found=.true.
            exit
         ! Second case:  We're at the end of the list.
         else if (.not. associated(node%next)) then
            exit
         ! General case:  Go to the next node in the list.
         else
            node=>node%next
         end if
      end do
      if (.not. found) then
         print*,'ERROR, cannot find requested field and xlvl!'
         print*,'Looked for ',trim(fieldName),' ',fieldXLvl
         stop
      end if

      return
   end function findFieldWPSList

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:
   ! destroyFieldWPSList
   !
   ! DESCRIPTION:
   ! Public destructor method for FieldWPSList data type.
   !
   !---------------------------------------------------------------------------

   subroutine destroyFieldWPSList(this)

      ! Arguments
      type(FieldWPSList), intent(inout) :: this

      ! Local variables
      type(FieldWPSNode), pointer :: node
      logical :: done

      ! If list is empty, there's nothing to do.
      if (.not. associated(this%head)) return

      ! Loop through the list from tail to head, deallocating node by node.
      done=.false.
      do
         if (done) exit

         node=>this%tail

         ! Special case of list with one node
         if (.not. associated(node%previous)) then
            call destroyFieldWPS(node%field)
            deallocate(node%field)
            nullify(node%field)
            nullify(node%previous)
            deallocate(node)
            nullify(this%head)
            nullify(this%tail)
            done=.true.
         else
            ! Remove last node from list
            this%tail=>node%previous
            call destroyFieldWPS(node%field)
            deallocate(node%field)
            nullify(node%field)
            nullify(node%previous)
            deallocate(node)
         end if
      end do

      return
   end subroutine destroyFieldWPSList

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:
   ! getNamesXlvlsFieldWPSList
   !
   ! DESCRIPTION:
   ! Public method for getting xlvls from FieldWPSList data type.
   !
   !---------------------------------------------------------------------------

   subroutine getXlvlsFieldWPSList(this,numNodes,fieldXlvls)

      ! Arguments
      type(FieldWPSList),intent(in) :: this
      integer,intent(in) :: numNodes
      real, intent(out) :: fieldXlvls(numNodes)

      ! Local variables
      type(FieldWPSNode), pointer :: node
      integer :: count

      if (.not. associated(this%head)) then
         print*,'ERROR, linked list is empty!'
         stop 1
      end if

      node=>this%head
      count = 0
      do
         count = count+1
         if (count .gt. numNodes) then
            print*,'ERROR, linked list has more nodes then expected!'
            print*,'Expected ',numNodes
            print*,'Found ',count
            stop 1
         end if

         fieldXlvls(count) = node%field%xlvl
         if (.not. associated(node%next)) then
            exit
         else
            node=>node%next
         end if
      end do

      if (count .ne. numNodes) then
         print*,'ERROR, linked list has different node count then expected!'
         print*,'Expected ',numNodes
         print*,'Found ',count
         stop 1
      end if

      return
   end subroutine getXlvlsFieldWPSList

end module FieldWPSList_mod
