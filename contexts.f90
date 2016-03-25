!!! Copyright (C) 2016  Tom Hanika (mail@tomhanika.de
!!! This file is part of fcatran.
! fcatran is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.

! fcatran is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with fcatran.  If not, see <http://www.gnu.org/licenses/>.

module contexts
use iso_fortran_env, only: int64
use rndmod
implicit none 
private

!! public parameters ans constants
public:: pr,cl,lk,ik
!! public types
public:: aset, oset, context,implication,list_of_implications,list_of_formal_concepts
!! public functions and subroutines
public::create_context, create_random_context, put,der,create_attribute_set,create_object_set,&
     is_concept,add_to_attribute_seti,create_empty_attribute_set,&
     equal_up_to_i,add_implication,create_implication,is_implication,a_imp_conc,stem_base,size_of,&
     &create_ur_context,create_gcd_context,create_bool_context,all_intents,create_full_context,&
     &trash_list_of_implications,alter_context_ij,create_empty_context,has_cross,all_formal_concepts,&
     &trash_list_of_fc,size_of_list_of_fc,all_pi_induced_concepts,loc_max_context,list_of_contexts,&
     &crosses,get,number_of_attributes,number_of_objects,c_put

!! public operators
public:: operator(<=),operator(<),operator(==),operator(>=),operator(>),operator(.cap.),operator(.cup.),operator(+),assignment(=)

integer, parameter:: clength=50
integer, parameter:: pr = selected_real_kind(16,300)
integer, parameter:: ik = 8 !! Integer kind !FixMe
integer, parameter:: lk = 1 !! Logical kind
integer, parameter:: cl = 32 !! character len
logical           :: rnd_initialized=.FALSE.

!! TYPES
type object
   character(len=cl)::name
end type object

type attribute
   character(len=cl)::name
end type attribute

type context
   private
   type(object), dimension(:), allocatable::  soo     !! vector of objects
   type(attribute), dimension(:), allocatable::  soa !! vector of attributes
   logical(lk), dimension(:,:), allocatable:: incidence !! incidence relation
end type context

type aset !! a (sub)set of attributes of a context, represented by a logical vector of the length of the vector of attributes of the associated context. 
   private
   type(context), pointer:: c=> null()
   logical(lk), dimension(:), allocatable:: vec
end type aset

type oset !! a (sub)set of attributes of a context, represented by a logical vector of the length of the vector of attributes of the associated context. 
   private
   type(context), pointer:: c=> null()
   logical(lk), dimension(:), allocatable:: vec
end type oset

type implication !! a implications, 2 sets of attributes (1st implying the second) in the *same* context. 
   type(context), pointer:: c=> null()
   type(aset):: A,B
end type implication

type list_of_implications
   integer(ik)::amount=0
   type(implication), pointer:: impl => null ()
   type(list_of_implications), pointer:: next => null(), prev =>null()
end type list_of_implications

type formal_concept
   type(context), pointer:: c => null()
   type(oset)::os
   type(aset)::as
end type formal_concept

type list_of_formal_concepts
   integer(ik)::amount=0
   type(formal_concept ), pointer:: fc => null()
   type(list_of_formal_concepts), pointer:: next => null(), prev => null()
end type list_of_formal_concepts

type list_of_contexts
   type(context), pointer:: elem
   type(list_of_contexts), pointer:: next => null()
   type(list_of_contexts), pointer:: prev => null()
end type list_of_contexts

!!INTERFACES:
interface put
   module procedure:: put_c,put_aset,put_oset,put_iset,put_impl,put_fc,put_c_file
end interface put

interface c_put 
   module procedure:: c_put_c,c_put_c_file
end interface c_put

interface get
   module procedure:: get_c_file
end interface get

interface assignment (=)
   module procedure:: assign_ob_char,assign_at_char, assign_aset,assign_context,assign_fc
end interface assignment (=)

interface der
   module procedure:: der_a, der_o
end interface der

interface size_of
   module procedure:: size_of_list_of_implications,size_of_list_of_fc,size_of_list_of_implications_short
end interface size_of
interface create_attribute_set
   module procedure:: create_attribute_setl,create_attribute_seti
end interface create_attribute_set

!! Operators
interface operator (<)
   module procedure:: a_subset,o_subset
end interface operator (<)

interface operator (>)
   module procedure:: a_supset,o_supset
end interface operator (>)

interface operator (<=)
   module procedure:: a_subseteq,o_subseteq
end interface operator (<=)

interface operator (>=)
   module procedure:: a_supseteq,o_supseteq
end interface operator (>=)

interface operator (==)
   module procedure:: a_equal,o_equal,fc_equals_fc, contex_equals_context
end interface operator (==)

interface operator (/=)
   module procedure:: a_not_equal,o_not_equal
end interface operator (/=)

interface operator (.cap.)
   module procedure:: a_intersect,o_intersect
end interface operator (.cap.)

interface operator (.cup.)
   module procedure:: a_union,o_union
end interface operator (.cup.)

interface operator (+)
   module procedure:: aset_plus_i
end interface operator (+)


contains

subroutine put_c(c)
  type(context), intent(in)::c
  integer::i,j 
  write(*,*) "---------------"
  do i=1,size(c%incidence,1)
     do j=1,size(c%incidence,2)
        if(c%incidence(i,j)) then 
           write(*,FMT='(A1)',advance='no') 'X'
        else
           write(*,FMT='(A1)',advance='no') '.'
        end if
     end do
     write(*,*)
  end do
  write(*,*) "---------------"
end subroutine put_c

subroutine put_aset(a)
  type(aset), intent(in):: a
  integer:: i
!  do i=1,size(a%vec)   !! FIX ME LATER FOR OUTPUT OF attributes..
!     if(a%vec(i)) write(*,*) a%c%soa(i)  !! 
!  end do
  write(*,*) a%vec
end subroutine put_aset

subroutine put_oset(o)
  type(oset), intent(in):: o
  integer:: i
!  do i=1,size(a%vec)
!     if(a%vec(i)) write(*,*) a%c%soa(i)  !! 
!  end do
  write(*,*) o%vec
end subroutine put_oset

subroutine put_impl(i)
type(implication), intent(in):: i
call put(i%A)
write(*,*) '->'
call put(i%B)
end subroutine put_impl

subroutine put_fc(fc)
type(formal_concept), intent(in):: fc
call put(fc%os)
write(*,*) ';'
call put(fc%as)
end subroutine put_fc

subroutine put_iset(i)
  type(list_of_implications), target,intent(in):: i
  type(list_of_implications), pointer:: tmp
  if(associated(i%impl)) then 
     call put(i%impl%A)
     write(*,*) ' -> '
     call put(i%impl%B)
     tmp => i
     do while(associated(tmp%next))
        write(*,*) ''
        call put(tmp%next%impl%A)
        write(*,*) ' -> '
        call put(tmp%next%impl%B)
        tmp => tmp%next
     end do
  else
     write(*,*) 'Empty implication set.'
  end if
end subroutine put_iset

subroutine put_c_file(c,file,form)
  type(context), intent(in):: c
  character(len=*), intent(in):: file
  character(len=1),intent(in), optional:: form
  integer:: i,j
  character(len=32):: outputchar
  open(42,file=trim(file),status='unknown')
  
  
  if(present(form)) then
     if(form=='B') then 
        write(42,fmt='(A1)') 'B' !! FIXME blanks
        write(42,*) trim(file)
        write(42,*) size(c%incidence,1) !! FIXME blanks
        write(42,*) size(c%incidence,2) !! FIXME blanks
        !Write out objects
        do i=1,size(c%soo)
           write(42,*) c%soo(i)
        end do
        !Write out attributs
        do i=1,size(c%soa)
           write(42,*) c%soa(i)
        end do
     elseif(form=='A') then 
        write(42,fmt='(A1)') 'A'
        write(42,*) size(c%incidence,1)
        write(42,*) size(c%incidence,2)
     end if
  else
     write(42,'(A1)') 'A'
     write(outputchar,*) size(c%incidence,1)
     write(42,*) trim(adjustl(outputchar)) !!size(c%incidence,1)
     write(outputchar,*) size(c%incidence,2)
     write(42,*) trim(adjustl(outputchar)) !!size(c%incidence,2)
  end if
  do i=1,size(c%incidence,1)
     do j=1,size(c%incidence,2)
        if(c%incidence(i,j)) then 
           write(42,fmt='(A1)',advance='no') 'X'
        else
           write(42,fmt='(A1)',advance='no') '.'
        end if
     end do
     write(42,*)
  end do

end subroutine put_c_file

subroutine c_put_c(con)
  use iso_c_binding
  interface
     subroutine testoutput(g,m,L) bind(C)
       import
       integer(c_int), value, intent(in) :: g,m
       logical (C_BOOL)::L(*)
     end subroutine testoutput
  end interface
  type(context), intent(in):: con
  logical(kind=C_BOOL), dimension(size(con%incidence,1),size(con%incidence,2)), target :: L 
  integer:: g,m
  g=size(con%incidence,1)
  m=size(con%incidence,2)
  L = con%incidence
  call testoutput(g,m,L) 
end subroutine c_put_c

subroutine c_put_c_file(con,filename)
  use iso_c_binding
  interface
     subroutine cc_put_c_file(g,m,L, filename) bind(C,name="c_put_c_file")
       import
       integer(c_int), value, intent(in) :: g,m
       logical (c_bool), intent(in)      ::L(*)
       character(len=1, kind=c_char), dimension(*), intent(in)::filename
     end subroutine cc_put_c_file
  end interface
  type(context), intent(in):: con
  character(*), intent(in):: filename
  logical(kind=C_BOOL), dimension(size(con%incidence,1),size(con%incidence,2)), target :: L 
  integer:: g,m
  g=size(con%incidence,1)
  m=size(con%incidence,2)
  L = con%incidence
  call cc_put_c_file(g,m,L,filename // char(0)) 
end subroutine c_put_c_file

subroutine get_c_file(c,filename)
  type(context), intent(out):: c
  character(len=*), intent(in):: filename
  character(len=1):: typeof
  character(len=1):: temp
  character(cl), dimension(:), allocatable::o,a
  integer, dimension(2)::dims
  logical(lk), dimension(:,:), allocatable::inci
  integer::i,k
  
  open(43,file=filename,status='old', action='read')
  
  read(43,*) typeof
  if(typeof/="A") then 
     write(*,*) "Fatal error, non-anonymous burmeister context format not implemented, yet"
  end if
  
  write(*,*) "Type is:",typeof
  read(43,*) dims(1)
  read(43,*) dims(2)
  write(*,*) "Dim 1 is:",dims(1), "Dim 2 is",dims(2)
  allocate(o(dims(1)))
  allocate(a(dims(2)))
  allocate(inci(dims(1),dims(2)))
  o='' !! since this is anonymous context, no need for names. 
  a=''
  do i=1,dims(1)
     do k=1,dims(2)-1
        read(43,fmt='(A1)',advance='no') temp
        inci(i,k) = temp == 'X'
     end do
     read(43,fmt='(A1)',advance='yes') temp
     inci(i,dims(2)) = temp == 'X'
  end do
  close(43)
  c=create_context(o,a,inci)
end subroutine get_c_file

type(context) function create_context(o,a,m)  !! slow, does copy
  character(cl), dimension(:), intent(in):: o
  character(cl), dimension(:), intent(in):: a
  logical(lk), dimension(:,:), intent(in):: m
  integer:: i
  allocate(create_context%soo(1:size(o))) !! allocate in context vectors for o and a
  allocate(create_context%soa(1:size(a))) 
  allocate(create_context%incidence(1:size(m,1),size(m,2)))
  do i=1,size(o)
     create_context%soo(i)=o(i)
  end do
  do i=1,size(a)
     create_context%soa(i)=a(i)
  end do
  create_context%incidence=m
end function create_context

type(context) function create_random_context(no,na,prop,type)
  integer, intent(in):: no,na
  real(pr), intent(in):: prop
  integer, intent(in), optional:: type
  real(pr), dimension(na):: vec 
  integer:: i
  
  allocate(create_random_context%soo(1:no)) !! allocate in context vectors for o and a
  allocate(create_random_context%soa(1:na)) 
  allocate(create_random_context%incidence(1:no,1:na))
  
  if(.not. rnd_initialized) THEN 
     CALL init_random_seed()
     rnd_initialized=.TRUE.
  end if
  
  do i=1,no !dir$ parallel
     call random_number(vec)
     create_random_context%incidence(i,:) = vec<prop  !! whereever in rnd vector an entry is < prop, there'll be a T
  end do
  
  do i=1,no
     create_random_context%soo(i)=''
  end do
  do i=1,na
     create_random_context%soa(i)=''
  end do
end function create_random_context

type(context) function create_ur_context(m,n) !! upper-right
  integer, intent(in):: m,n
  integer ::i,j
  allocate(create_ur_context%soo(1:m))
  allocate(create_ur_context%soa(1:n))
  allocate(create_ur_context%incidence(1:m,1:n))
  create_ur_context%incidence=.false.
  do i=1,m
     do j=i+1,n
        create_ur_context%incidence(i,j) = .true.
     end do
  end do
end function create_ur_context

type(context) function create_bool_context(m) !! 
  integer, intent(in):: m
  integer ::i,j
  allocate(create_bool_context%soo(1:m))
  allocate(create_bool_context%soa(1:m))
  allocate(create_bool_context%incidence(1:m,1:m))
  create_bool_context%incidence=.true.
  do i=1,m
        create_bool_context%incidence(i,i) = .false.
  end do
end function create_bool_context

pure type(context) function create_empty_context(o,a) !! 
  integer, intent(in):: o,a
  allocate(create_empty_context%soo(1:o))
  allocate(create_empty_context%soa(1:a))
  allocate(create_empty_context%incidence(1:o,1:a))
  
  create_empty_context%incidence = .false.
  
end function create_empty_context

type(context) function create_full_context(m) !! upper-right
  integer, intent(in):: m
  integer ::i,j
  allocate(create_full_context%soo(1:m))
  allocate(create_full_context%soa(1:m))
  allocate(create_full_context%incidence(1:m,1:m))
  create_full_context%incidence=.true.
end function create_full_context

type(context) function create_gcd_context(m,n) !! upper-right
  integer, intent(in):: m,n
  integer ::i,j
  allocate(create_gcd_context%soo(1:m))
  allocate(create_gcd_context%soa(1:n))
  allocate(create_gcd_context%incidence(1:m,1:n))
  create_gcd_context%incidence=.false.
  do i=1,m
     do j=1,n
        create_gcd_context%incidence(i,j) = gcd_rec(i,j)==1
     end do
  end do
end function create_gcd_context


!! ROSETTA-CODE for gcd
recursive function gcd_rec(u, v) result(gcd)
    integer             :: gcd
    integer, intent(in) :: u, v
 
    if (mod(u, v) /= 0) then
        gcd = gcd_rec(v, mod(u, v))
    else
        gcd = v
    end if
end function gcd_rec

type(aset) function create_attribute_setl(c,lvec)
  type(context), intent(in), target::c
  logical, dimension(:),  intent(in)::lvec
  integer:: minimum
  
  if(allocated(c%incidence)) then 
     create_attribute_setl%c => c
     if(size(lvec)==size(c%incidence,1)) then  !!if lvec correct, use it
        allocate(create_attribute_setl%vec(1:size(c%incidence,2)))
        create_attribute_setl%vec = lvec
     else !! if not, allocate logical vector in aset result, and copy entries up to min length of both
        allocate(create_attribute_setl%vec(size(c%incidence,2)))
        minimum = min(size(c%incidence,1),size(lvec))
        create_attribute_setl%vec=.false.
        create_attribute_setl%vec(1:minimum) = lvec(1:minimum)
     end if
  end if
end function create_attribute_setl

type(aset) function create_empty_attribute_set(c)
  type(context), intent(in), target::c
  integer:: minimum
  
  if(allocated(c%incidence)) then  !! FIXME: what if not?
     create_empty_attribute_set%c => c
     allocate(create_empty_attribute_set%vec(size(c%incidence,2)))
     create_empty_attribute_set%vec=.false.
  end if
end function create_empty_attribute_set

type(aset) function create_full_attribute_set(c)
  type(context), intent(in), target::c
  integer:: minimum
  
  if(allocated(c%incidence)) then  !! FIXME: waht if not?
     create_full_attribute_set%c => c
     allocate(create_full_attribute_set%vec(size(c%incidence,2)))
     create_full_attribute_set%vec=.true.
  end if
end function create_full_attribute_set


type(aset) function create_attribute_seti(c,i)
  type(context), intent(in), target::c
  integer, intent(in):: i 
  
  if(allocated(c%incidence)) then 
     create_attribute_seti%c => c
     allocate(create_attribute_seti%vec(size(c%incidence,2)))
     create_attribute_seti%vec=.false.
     if(i>0) then 
        create_attribute_seti%vec(i)=.true.
     end if
  end if
end function create_attribute_seti

type(aset) function add_to_attribute_seti(a,i)
  type(aset), intent(inout):: a
  integer, intent(in):: i 
  
  if(associated(A%c) .and. i<=size(a%vec)) then 
     a%vec(i)=.true.
  else
     write(*,*) "Couldn't add i=",i
  end if
end function add_to_attribute_seti


type(oset) function create_object_set(c,lvec)
  type(context), intent(in), target::c
  logical, dimension(:),  intent(in)::lvec
  integer:: minimum
  
  if(allocated(c%incidence)) then 
     create_object_set%c => c
     if(size(lvec)==size(c%incidence,2)) then  !!if lvec correct, use it
        allocate(create_object_set%vec(size(c%incidence,1)))
        create_object_set%vec = lvec
     else !! if not, allocate logical vector in aset result, and copy entries up to min length of both
        allocate(create_object_set%vec(size(c%incidence,1)))
        minimum = min(size(c%incidence,2),size(lvec))
        create_object_set%vec=.false.
        create_object_set%vec(1:minimum) = lvec(1:minimum)
     end if
  end if
end function create_object_set

type(implication) function create_implication(a,b)
  type(aset), intent(in)::a,b
  if(associated(a%c, target=b%c)) then 
     create_implication%c => a%c
     create_implication%a = a
     create_implication%b = b
  else 
     STOP !! as requested by Daniel 
  end if
end function create_implication



type(oset) function der_a(as)
  type(aset), target, intent(in)::as
  integer:: i
  integer:: j
  !point context pointer of result to context pointer of as
  der_a%c => as%c
  !allocate object_set vector with lenght incidence, dim 1
  allocate(der_a%vec(size(as%c%incidence,1)))
  
  !If aset is a empty set (all entries are FALSE), return whole object set (all Entries are TRUE)
  if(.not. any(as%vec)) then 
     der_a%vec=.true.
  else
     do i=1, size(as%c%soo) !! iterate through all objects 
        der_a%vec(i)=.true.   !! assume object is in set
        do j=1,size(as%vec) !! iterate through all attributes in as
           if(.not. as%vec(j)) cycle  !! if attribute is not in attribute set, cycle
           if(.not. as%c%incidence(i,j)) then !!attribute is in a set but incidence is false
              der_a%vec(i)=.false.
              exit
           end if
        end do
     end do
  end if
end function der_a

type(aset) function der_o(os)
  type(oset), intent(in)::os
  integer:: i
  integer:: j
  !point context pointer of result to context pointer of as
  der_o%c => os%c
  !allocate attribute_set vector with length incidence, dim 2
  allocate(der_o%vec(size(os%c%incidence,2)))
  !If os is a empty set (all entries are FALSE), return whole attribute set (all Entries are TRUE)
  if(.not. any(os%vec)) then 
     der_o%vec=.true.
  else
     do i=1, size(os%c%soa) !! iterate through all attributes 
        der_o%vec(i)=.true.   !! assume object is in set
        do j=1,size(os%vec) !! iterate through all objects in os
           if(.not. os%vec(j)) cycle  !! if attribute is not in attribute set, cycle
           if(.not. os%c%incidence(j,i)) then !!attribute is in a set but incidence is false
              der_o%vec(i)=.false.
              exit
           end if
        end do
     end do
  end if
end function der_o

logical function is_concept(a,b)
  type(oset), intent(in):: a
  type(aset), intent(in):: b
  is_concept=.false.
  if(associated(a%c, target=b%c)) is_concept = der(a) == b .and. a == der(b)
end function is_concept

logical function is_implication(a,b)
  type(aset), intent(in):: a,b
  is_implication = A<= der(der(B))
end function is_implication


!! Operator routines (a lot of set operations) 
pure logical function a_subseteq(a,b)
  type(aset), intent(in):: a,b
  integer:: i
  a_subseteq = .false.
  if(associated(a%c,target=b%c) .and. size(a%vec)==size(b%vec)) then
     a_subseteq=.true.
     do i=1,size(b%vec)
        if(a%vec(i)) then   !!
           if(b%vec(i)) then 
           else
              a_subseteq=.false.
           end if
        end if
     end do
  end if
end function a_subseteq

pure logical function a_equal(a,b)
  type(aset), intent(in):: a,b
  integer:: i
  a_equal=.false.
  if(associated(a%c,target=b%c)) then 
     a_equal=.true.
     do i=1, size(b%vec)
        if(b%vec(i).neqv. a%vec(i)) then 
           a_equal=.false.
        end if
     end do
  end if
end function a_equal

pure logical function a_not_equal(a,b)
  type(aset), intent(in):: a,b
  integer:: i
  a_not_equal=.false.
  if(associated(a%c,target=b%c)) then 
     a_not_equal= .not. a==b
  end if
end function a_not_equal

pure logical function a_subset(a,b)
  type(aset), intent(in):: a,b
  a_subset=.false.
  if(associated(a%c,target=b%c)) then
     if(.not. a == b .and. a <= b) then 
        a_subset=.true.
     end if
  end if
end function a_subset

pure logical function a_supset(a,b)
  type(aset), intent(in):: a,b
  a_supset=.false.
  a_supset= b < a
end function a_supset

pure logical function a_supseteq(a,b)
  type(aset), intent(in):: a,b
  a_supseteq=.false.
     a_supseteq= b <= a
   end function a_supseteq
   
pure logical function o_subseteq(a,b)
  type(oset), intent(in):: a,b
  integer:: i
  o_subseteq = .false.
  if(associated(a%c,target=b%c)) then
     o_subseteq=.true.
     do i=1,size(b%vec)
        if(a%vec(i)) then   !!
           if(b%vec(i)) then 
           else
              o_subseteq=.false.
              exit
           end if
        end if
     end do
  end if
end function o_subseteq

pure logical function o_equal(a,b)
  type(oset), intent(in):: a,b
  integer:: i
  o_equal=.false.
  if(associated(a%c,target=b%c)) then 
     o_equal=.true.
     do i=1, size(b%vec)
        if(b%vec(i).neqv. a%vec(i)) then 
           o_equal=.false.
           exit
        end if
     end do
  end if
end function o_equal

pure logical function o_not_equal(a,b)
  type(oset), intent(in):: a,b
  integer:: i
  o_not_equal=.false.
  if(associated(a%c,target=b%c)) then 
     o_not_equal=.not. a==b
  end if
end function o_not_equal

pure logical function o_subset(a,b)
  type(oset), intent(in):: a,b
  o_subset=.false.
  if(associated(a%c,target=b%c)) then
     if(.not. a == b .and. a <= b) then 
        o_subset=.true.
     end if
  end if
end function o_subset

pure logical function o_supset(a,b)
  type(oset), intent(in):: a,b
  o_supset=.false.
  if(b < a) then 
     o_supset=.true.
  end if
end function o_supset

pure logical function o_supseteq(a,b)
  type(oset), intent(in):: a,b
  o_supseteq=.false.
  if(b <= a) then 
     o_supseteq=.true.
  end if
end function o_supseteq

type(aset) function a_intersect(a,b)
  type(aset), intent(in):: a,b
  integer:: i
  if(associated(a%c,target=b%c)) then
     a_intersect%c=> a%c         !! Result shall use the same context
     allocate(a_intersect%vec(size(a%vec))) !! since creation of attribute sets shall only be done by method, same  length for both vectors is guranteed. 
     do i=1,size(a%vec)
        a_intersect%vec(i) = a%vec(i) .and. b%vec(i)
     end do
  else 
     allocate(a_intersect%vec(1))
     a_intersect%vec(1)=.false.
  end if
end function a_intersect

type(aset) function a_union(a,b)
  type(aset), intent(in):: a,b
  integer:: i
  if(associated(a%c,target=b%c)) then
     a_union%c=> a%c         !! Result shall use the same context
     allocate(a_union%vec(size(a%vec))) !! since creation of attribute sets shall only be done by method, same  length for both vectors is guranteed. 
     do i=1,size(a%vec) !dir$ parallel
        a_union%vec(i) = a%vec(i) .or. b%vec(i)
     end do
  else 
     allocate(a_union%vec(1))
     a_union%vec(1)=.false.
  end if
end function a_union

type(oset) function o_intersect(a,b)
  type(oset), intent(in):: a,b
  integer:: i
  if(associated(a%c,target=b%c)) then
     o_intersect%c=> a%c         !! Result shall use the same context
     allocate(o_intersect%vec(size(a%vec))) !! since creation of objects sets shall only be done by method, same  length for both vectors is guranteed. 
     do i=1,size(a%vec)
        o_intersect%vec(i) = a%vec(i) .and. b%vec(i)
     end do
  else 
     allocate(o_intersect%vec(1))
     o_intersect%vec(1)=.false.
  end if
end function o_intersect

type(oset) function o_union(a,b)
  type(oset), intent(in):: a,b
  integer:: i
  if(associated(a%c,target=b%c)) then
     o_union%c=> a%c         !! Result shall use the same context
     allocate(o_union%vec(size(a%vec))) !! since creation of attribute sets shall only be done by method, same  length for both vectors is guranteed. 
     do i=1,size(a%vec)
        o_union%vec(i) = a%vec(i) .and. b%vec(i)
     end do
  else 
     allocate(o_union%vec(1))
     o_union%vec(1)=.false.
  end if
end function o_union

pure logical function equal_up_to_i(a,b,i)
  type(aset), intent(in) :: a,b
  integer, intent(in)    :: i
  integer                :: j
  equal_up_to_i=.false.
  if(associated(a%c, target=b%c)) then 
     if(i==0) then 
        equal_up_to_i=.true.
     elseif(size(a%vec) >= i .and. size(b%vec) >=i) then 
        equal_up_to_i=.true.
        do j=1,i
           if(a%vec(j) .neqv. b%vec(j)) then 
              equal_up_to_i=.false.
              exit
           end if
        end do
     end if
  end if
end function equal_up_to_i

type(aset) function aset_plus_i(a,i) !! adds attribute i and removes all attributes >i
  type(aset), intent(in)::a
  integer, intent(in)   ::i
  
  aset_plus_i = a
  aset_plus_i%vec(i) = .true.
  if(size(a%vec) > i) then 
     aset_plus_i%vec(i+1:size(a%vec))=.false.
  end if
end function aset_plus_i

type(list_of_implications) function stem_base(c)
  type(context), intent(in):: c
  type(aset) :: a,tmpa, tmpb
  integer::i, j

  if(allocated(c%incidence)) then 
     a=create_empty_attribute_set(c)
     do 
        if(a /= der(der(a))) then  ! add a->a'' if a /=a''
           call add_implication(stem_base, create_implication(a,der(der(a))))  
        end if
        do i=size(c%incidence,2), 0, -1 !! starting from i=|M|, decrement by 1
           j=i
           if(i/=0 .and. is_false(a,i)) then  !! i/=0 is necessary, since it will segfault..
              tmpa = a+i   !! set a(i)=.true. and everything after it, .false. 
              tmpb = a_imp_conc(tmpa,stem_base)
              if(equal_up_to_i(a,tmpb,i-1)) exit  !! both have to be equal up to i-1, actually
           end if
        end do
        if(j==0) exit 
        a=tmpb
     end do
  end if
end function stem_base


!!Some misuse of list_of_implications for all_intents. Since we are only interested in the premise...
type(list_of_implications) function all_intents(c)
  type(context), intent(in):: c
  type(aset) :: a,tmpa, tmpb, full
  integer::i, j

  if(allocated(c%incidence)) then 
     full=create_full_attribute_set(c)
     a=create_empty_attribute_set(c)
     !! first concept intent is \emptyset''
     a=der(der(a))
     call add_implication(all_intents, create_implication(a,a))
     if(a /= full) then  !! necessary for preventing segfault in full-context
        do 
           do i=size(c%incidence,2), 0, -1 !! starting from i=|M|, decrement by 1
              j=i
              if(i/=0 .and. is_false(a,i)) then  !! i \in M\subset A (is_false)
!!                 tmpa = a+i   !! set a(i)=.true. and everything after it, .false. 
!!                 tmpb = der(der(tmpa))   !! 
                 tmpb = der(der(a+i))   !! optimized code
                 if(equal_up_to_i(a,tmpb,i-1)) exit  !! both have to be equal up to i-1, actually
              end if
           end do
           call add_implication(all_intents, create_implication(tmpb,tmpb))
           if(tmpb == full) exit 
           a=tmpb
        end do
     end if
  end if
end function all_intents

!! does NOT verify, yet, just creates the type. Maybe some optional verify would be nice
type(formal_concept) function create_formal_concept(a,b)
  type(oset), intent(in):: a
  type(aset), intent(in):: b
  if(associated(a%c, target=b%c)) then 
     create_formal_concept%c  => a%c
     create_formal_concept%os = a
     create_formal_concept%as = b
  end if
end function create_formal_concept

type(list_of_formal_concepts) function all_formal_concepts(c)
  type(context), intent(in):: c
  type(aset) :: a,tmpa, tmpb, full
  integer::i, j

  if(allocated(c%incidence)) then 
     full=create_full_attribute_set(c)
     a=create_empty_attribute_set(c)
     !! first concept intent is \emptyset''
     a=der(der(a))
     call add_formal_concept(all_formal_concepts, create_formal_concept(der(a),a))

     if(a /= full) then  !! necessary for preventing segfault in full-context
        do 
           do i=size(c%incidence,2), 0, -1 !! starting from i=|M|, decrement by 1
              j=i
              if(i/=0 .and. is_false(a,i)) then  !! i \in M\subset A (is_false)
!!                 tmpa = a+i   !! set a(i)=.true. and everything after it, .false. 
!!                 tmpb = der(der(tmpa))   !! 
                 tmpb = der(der(a+i))   !! optimized
                 if(equal_up_to_i(a,tmpb,i-1)) exit  !! both have to be equal up to i-1, actually
              end if
           end do
           call add_formal_concept(all_formal_concepts, create_formal_concept(der(tmpb),tmpb))
           if(tmpb == full) exit 
           a=tmpb
        end do
     end if
  end if
end function all_formal_concepts


pure logical function is_false(a,i)
  type(aset), intent(in):: a
  integer, intent(in)   :: i
  
  is_false = .not. a%vec(i)
end function is_false
  

!! ASSIGN subroutines for assignment operator
subroutine assign_ob_char(o,cr)
  type(object), intent(INOUT)::o
  character(len=*), intent(in)::cr
  o%name=cr
end subroutine assign_ob_char

subroutine assign_at_char(a,cr)
  type(attribute), intent(INOUT)::a
  character(len=*), intent(in)::cr
  a%name=cr
end subroutine assign_at_char

subroutine assign_aset(A,B)
  type(aset), intent(inout)::a
  type(aset), intent(in)::b
  if(allocated(a%vec)) deallocate(a%vec)
  a%c => null()
  if(allocated(b%vec)) then 
     a%c => b%c
     allocate(a%vec(1:size(b%vec)))
     a%vec = b%vec
  end if
end subroutine assign_aset

pure subroutine assign_context(a,b)
  type(context), intent(inout):: a
  type(context), intent(in ):: b
  integer:: i

  if(allocated(a%soo)) deallocate(a%soo)
  if(allocated(a%soa)) deallocate(a%soa)
  if(allocated(a%incidence)) deallocate(a%incidence)
  if(allocated(b%soo)) then 
     allocate(a%soo(size(b%soo)))
     a%soo=b%soo
  end if
  if(allocated(b%soa)) then 
     allocate(a%soa(size(b%soa)))
     a%soa=b%soa
  end if
  if(allocated(b%incidence)) then 
     allocate(a%incidence(size(b%incidence,1),size(b%incidence,2)))
     !a%incidence = b%incidence !! ifort doesn't like this with big matrices
     do i=1,size(a%incidence,1)
        a%incidence(i,:) = b%incidence(i,:)
     end do
  end if
end subroutine assign_context

subroutine assign_fc(a,b)
  type(formal_concept), intent(out):: a
  type(formal_concept), intent(in) :: b
  a%c => b%c
  a%os = b%os
  a%as = b%as
end subroutine assign_fc

!! implications and list of implication routines

subroutine add_implication(list,imp)
  type(list_of_implications), target:: list
  type(implication), target  :: imp
  type(list_of_implications), pointer :: temp
  list%amount = list%amount + 1  !! increment amount of implications in list
  if(.not. associated(list%impl)) then 
     allocate(list%impl)
     list%impl = imp
  elseif(.not. associated(list%next)) then
     allocate(list%next)
     allocate(list%next%impl)
     list%next%impl = imp
     list%next%prev => list
  else
     ! temp => list%next
     ! do while(associated(temp%next))
     !    temp => temp%next
     ! end do
     ! allocate(temp%next)
     ! allocate(temp%next%impl)
     ! temp%next%impl = imp
     ! temp%next%prev => temp
     allocate(temp)
     allocate(temp%impl)
     temp%impl = imp
     temp%next => list%next
     list%next => temp
  end if
end subroutine add_implication

integer function size_of_list_of_implications(L)
  type(list_of_implications), target, intent(in):: L
  type(list_of_implications), pointer:: tmp

  if(.not. associated(L%impl)) then 
     size_of_list_of_implications=0
  elseif(.not. associated(L%next)) then 
     size_of_list_of_implications=1
  else
     size_of_list_of_implications =1
     tmp=>L
     do while(associated(tmp%next))
        size_of_list_of_implications = size_of_list_of_implications +1
        tmp => tmp%next
     end do
  end if
end function size_of_list_of_implications

integer function size_of_list_of_implications_short(L,shortcut)
  type(list_of_implications), target, intent(in):: L
  logical(lk):: shortcut
  size_of_list_of_implications_short = L%amount 
end function size_of_list_of_implications_short


subroutine trash_list_of_implications(L)
  type(list_of_implications), target:: L
  type(list_of_implications), pointer:: tmp,tmpb
  
  if(associated(L%impl)) then 
     tmp => L%next
     deallocate(L%impl)
     do while (associated(tmp)) 
        tmpb => tmp%next
        deallocate(tmp%impl)
        deallocate(tmp)
        tmp => tmpb
     end do
  end if
end subroutine trash_list_of_implications

integer function size_of_list_of_fc(L, shortcut)
  type(list_of_formal_concepts), target, intent(in):: L
  type(list_of_formal_concepts), pointer:: tmp
  logical(lk), optional:: shortcut
  
  if(present(shortcut)) then 
     size_of_list_of_fc = L%amount
  else
     if(.not. associated(L%fc)) then 
        size_of_list_of_fc=0
     elseif(.not. associated(L%next)) then 
        size_of_list_of_fc=1
     else
        size_of_list_of_fc =1
        tmp=>L
        do while(associated(tmp%next))
           size_of_list_of_fc = size_of_list_of_fc +1
           tmp => tmp%next
        end do
     end if
  end if
end function size_of_list_of_fc

subroutine trash_list_of_fc(L)
  type(list_of_formal_concepts), target:: L
  type(list_of_formal_concepts), pointer:: tmp,tmpb
  
  if(associated(L%fc)) then 
     tmp => L%next
     deallocate(L%fc)
     do while (associated(tmp)) 
        tmpb => tmp%next
        deallocate(tmp%fc)
        deallocate(tmp)
        tmp => tmpb
     end do
  end if
end subroutine trash_list_of_fc



!!Given an attribute set, and an set of implications, compute closure
! TODO: Remove already used implications (maybe prepare a copy or so) 
type(aset) function a_imp_conc(x,iset)
  type(aset) :: x
  type(list_of_implications), target, intent(in):: iset
  type(list_of_implications), pointer:: tmp
  type(aset) :: fp 
  a_imp_conc%c=> x%c  !! aset component c  is looking on the same context as input does
  
  
  a_imp_conc = x !! this is a closure operator. At least x  has to be returned. 
  fp = x         !!
  do 
     if(associated(iset%impl)) then 
        if(iset%impl%a <= a_imp_conc) then   !! 
           a_imp_conc = iset%impl%b .cup. a_imp_conc !! a_imp_conc contains already x ;)
        end if
        
        tmp => iset
        do while(associated(tmp%next)) 
!           call put(x)
!           call put(tmp%next%impl%a)
!           write(*,*) tmp%next%impl%a <= a_imp_conc
           if(tmp%next%impl%a <= a_imp_conc) then 
              a_imp_conc = tmp%next%impl%b .cup. a_imp_conc
           end if
           tmp => tmp%next
        end do
     end if
     if(fp == a_imp_conc) EXIT !! if there is no change after applying the implications, done!
     fp = a_imp_conc
  end do
end function a_imp_conc

subroutine alter_context_ij(c,i,j,val)
  type(context):: c
  integer, intent(in):: i,j
  logical(lk), intent(in)::val
  
  if(i>0 .and. i <= size(c%incidence,1) .and. j>0 .and. j <= size(c%incidence,2)) then 
     c%incidence(i,j) = val
  end if
end subroutine alter_context_ij

pure logical function has_cross(c,i,j)
  type(context), intent(in):: c
  integer, intent(in):: i,j
  
  has_cross = c%incidence(i,j)
end function has_cross

subroutine add_formal_concept(list,fc)
  type(list_of_formal_concepts), target:: list
  type(formal_concept), target  :: fc
  type(list_of_formal_concepts), pointer :: temp
  list%amount = list%amount + 1 
  
  if(.not. associated(list%fc)) then 
     allocate(list%fc)
     list%fc = fc
  elseif(.not. associated(list%next)) then
     allocate(list%next)
     allocate(list%next%fc)
     list%next%fc = fc
     list%next%prev => list
  else
     allocate(temp)
     allocate(temp%fc)
     temp%fc = fc
     temp%next => list%next
     list%next => temp
  end if
end subroutine add_formal_concept

!!! intersecting pseudo intents to create intents, functions to pull this off

pure logical function fc_equals_fc(a,b)
  type(formal_concept), intent(in):: a,b
  
  if(associated(a%c, target=b%c)) then 
     fc_equals_fc = a%os == b%os .and. a%as == b%as
  else
     fc_equals_fc = .false.
  end if
     
end function fc_equals_fc

pure logical(lk) function contex_equals_context(a,b)
  type(context), intent(in)::a,b
  contex_equals_context = all(a%incidence .eqv. b%incidence)
end function contex_equals_context

logical function fclist_contains_fc(fclist,fc)
  type(list_of_formal_concepts), intent(in):: fclist
  type(formal_concept), intent(in):: fc
  type(list_of_formal_concepts), pointer:: temp

  fclist_contains_fc = .false.
  if(associated(fclist%fc)) then 
     fclist_contains_fc = fclist%fc == fc  !! if this is true, the loop below will never start. 
     temp => fclist%next
     do while(.not. fclist_contains_fc .and. associated(temp))
        if(temp%fc == fc) then 
           fclist_contains_fc = .true. 
        end if
        temp => temp%next
     end do
  end if
end function fclist_contains_fc
  

!! Function gets an set of pseudo intent (list of implications) and computes *a* list of concepts by intersecting all pseudo intents pairwise with each other (There is a theorem stating that these gotta be intents). 
type(list_of_formal_concepts) function all_pi_induced_concepts(L)
  type(list_of_implications), target, intent(in):: L
  type(list_of_implications), pointer ::  pa,pb
  type(formal_concept) :: fc
  type(aset):: tmp_aset
  integer::i
  i=0
  pa => L !! pa looks at the head
  
  do while(associated(pa) .and. associated(pa%impl))   !! iterates through all implications, for every step
     pb => pa%next !! pb looks at head+1
     do while(associated(pb) .and. associated(pb%impl))!! pb iterates through all implications after pa
        tmp_aset = pa%impl%A .cap. pb%impl%A  !! intersection of 2 pseudo intents
        fc = create_formal_concept(der(tmp_aset), tmp_aset)
        if(.not. fclist_contains_fc(all_pi_induced_concepts,fc)) then 
           call add_formal_concept(all_pi_induced_concepts,fc)
        end if
        pb => pb%next
     end do
     pa => pa%next
  end do
  
  
end function all_pi_induced_concepts
 
recursive type(context) function loc_max_context(c, best) result(res) !
  type(context), intent(in)::c
  integer, intent(inout):: best
  type(context):: temp
  integer:: i,j, current, best_add,best_rm !! best_add: highest amount by adding one cross, etc.
  integer:: i_add_best,j_add_best,i_rm_best,j_rm_best
  
  best_add = best
  temp = c
  do i=1,size(temp%incidence,1)
     do j=1,size(temp%incidence,2)
        if(.not. has_cross(temp,i,j)) then 
           call alter_context_ij(temp,i,j,.TRUE._lk)
           current = size_of(stem_base(temp))
           if(current > best_add) then 
              best_add = current
              i_add_best = i
              j_add_best = j
           end if
           call alter_context_ij(temp,i,j,.FALSE._lk)
        end if
     end do 
  end do
     
  best_rm = best_add
  temp = c 
  do i=1,size(temp%incidence,1)
     do j=1,size(temp%incidence,2)
        if(has_cross(temp,i,j)) then 
           call alter_context_ij(temp,i,j,.FALSE._lk)
           current = size_of(stem_base(temp))
           if(current > best_rm) then 
              best_add = current
              i_rm_best = i
              j_rm_best = j
           end if
           call alter_context_ij(temp,i,j,.TRUE._lk)
        end if
     end do
  end do
  temp = c
  if(best_add > best_rm) then !! since best_rm is at least best, best_add is better than best
     best = best_add
     call alter_context_ij(temp,i_add_best,j_add_best,.TRUE._lk)
     res  = loc_max_context(temp, best)
  elseif(best_rm > best_add) then !! see above
     best = best_rm
     call alter_context_ij(temp,i_rm_best,j_rm_best,.FALSE._lk)
     res  = loc_max_context(temp, best)
  elseif(best_add > best) then !! in the case best_add == best_rm, use the add version
     best = best_add
     call alter_context_ij(temp,i_add_best,j_add_best,.TRUE._lk)
     res  = loc_max_context(temp, best)
  else
     res=c
  end if
end function loc_max_context

! !!create list_of_contexts wit 
! recursive type(list_of_contexts) function loc_max_contexts(c,best) res(result)
!   type(context), intent(in):: c
!   type(context):: con
!   integer, intent(in):: best
!   type(list_of_contexts), pointer:: temp
!   integer:: i,j,best_add, best_rm
!   con = c !! use copy of c
  

!   !!Idea:
!   !!1. build a list of contexts by 1-alteration that have at least best many p-intents
!   !!2. if there is no alteration that produces at least best many p-intents, return context
!   !!   from paramter list. 
!   do i=1,size(con%incidence,1)
!      do j=1,size(con%incidence,2)
!         if(.not. has_cross(temp,i,j)) then
!            call alter_context_ij(con,i,j,.true._lk)
!            best_add = size_of(stem_base(con))
!            if(best_add >= best) then 
!               !! ich brauch zuweisung für list_of_contexts
!               temp = loc_max_contexts(con,best_add)
!               if(.not. is_empty(temp)) then 
!                  !! ich brauch eine add function für die Liste
!                  call add_contexts(loc_max_contexts,temp)
!               end if
!            end if
!         end if
!      end do
!   end do
! end function loc_max_contexts

!! crosses: function returning the amount of crosses in a context

pure integer function crosses(c)
  type(context), intent(in):: c
  
  crosses=count(c%incidence)
end function crosses

pure integer(ik) function number_of_attributes(c)
  type(context), intent(in)::c
  
  number_of_attributes = size(c%incidence,2)
end function number_of_attributes

pure integer(ik) function number_of_objects(c)
  type(context), intent(in)::c
  
  number_of_objects = size(c%incidence,1)
end function number_of_objects


end module contexts


