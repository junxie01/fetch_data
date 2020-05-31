! fetch seed data
! 2018/03/09
program main
parameter (nn=21048576)
integer i,j,nsta,error,id
integer byear,eyear,bday,eday,iyear,iday,bmonth,emonth
integer month_bday,month_eday
integer imonth,nerr
integer ibmonth,iemonth
integer ibday,ieday,icom,jmon
integer do_decimate,do_rm_response,do_extract,do_resp
integer n_sta,jday,jday1
integer npts,nzero,imark,n1,nlen
integer nzhour,nzmin,nzsec,nzmsec
real f1,f2,dt0
real sigall(nn),sig(nn),sigo(nn)
real dt,beg,stla,stlo
integer,dimension(12):: monday=(/31,28,31,30,31,30,31,31,30,31,30,31/)
character(10),dimension(3000) :: stalist,netlist
character(80)  :: args,user,label
character(80)  :: sta_list
character(3)   :: component
character(3)   :: com(3),co
character(80)  :: chars,seed,mdata
character(100) :: mail,saclist
character(180) :: command
character(180) :: sac_tmp,sac,tmp
character(180) :: para,file,output_seed
character(80)  :: dirout,bash,resp
character(10)  :: year_day
character(2)   :: kh,kho,khole
logical ext
component="BHZ"
inquire(file="for_cc",exist=ext)
if(.not.ext)stop "for_cc file does not exist."
if(iargc().ne.5)stop 'Usage: fetch_data station.lst yearb dayb yeare daye'
if(verbos.eq.1)then
   write(*,*)'for_fetch:'
   write(*,*)'component icom download resp'
endif
call getarg(1,sta_list)
call getarg(2,tmp)
read(tmp,'(bn,i20)')byear
call getarg(3,tmp)
read(tmp,'(bn,i20)')bday
call getarg(4,tmp)
read(tmp,'(bn,i20)')eyear
call getarg(5,tmp)
read(tmp,'(bn,i20)')eday
open(9,file="for_cc")
!read(9,'(a80)')sta_list
read(9,*)co,icom,do_resp
close(9)
if(icom.eq.1)then
   com(1)=co
else
   com(1)=trim(co)//'Z'
   com(2)=trim(co)//'N'
   com(3)=trim(co)//'E'
endif

if(byear.gt.eyear)stop 'check the year'
if(byear.eq.eyear.and.bday.gt.eday) stop 'check the day'
user="tmp"
label="tmp"
inquire(file=sta_list,exist=ext)
if(.not.ext)stop 'check station list'
! read station list
i=1
open(13,file=sta_list)
do i=1,3000
   read(13,*,end=9,err=9)netlist(i),stalist(i)
enddo
9 close(13)
n_sta=i-1 !number of stations
! find day of month at begin time
if(mod(byear,4)==0.and.mod(byear,100).ne.0.or.mod(byear,400).eq.0)monday(2)=29  
month_bday=0
i=1
10 month_bday=month_bday+monday(i)
if(bday.le.month_bday)then
   bmonth=i
else
   i=i+1
   goto 10
endif
month_bday=bday-(month_bday-monday(i))
!write(*,*)byear,bmonth,month_bday

! find day of month at end time
if(mod(eyear,4)==0.and.mod(eyear,100).ne.0.or.mod(eyear,400).eq.0)monday(2)=29  
month_eday=0
i=1
11 month_eday=month_eday+monday(i)
if(eday.le.month_eday)then
   emonth=i
else
   i=i+1
   goto 11
endif
month_eday=eday-(month_eday-monday(i))
!write(*,*)eyear,emonth,month_eday
call system("mkdir seed 2>/dev/null")
call system("mkdir resp 2>/dev/null")
call system("mkdir mdat 2>/dev/null")
do iyear=byear,eyear ! loop ver year
   monday(2)=28
   if(mod(iyear,4)==0.and.mod(iyear,100).ne.0.or.mod(iyear,400).eq.0)monday(2)=29  
   ibmonth=1
   iemonth=12
   if(iyear.eq.byear)ibmonth=bmonth
   if(iyear.eq.eyear)iemonth=emonth
   jday=0
   do imonth=ibmonth,iemonth ! loop over month
      jday1=0
      if(imonth.eq.1)then
         jday1=0       
      else
         do jmon=1,imonth-1
            jday1=jday1+monday(jmon)
         enddo
      endif
      ibday=1
      ieday=monday(imonth)
      if(iyear.eq.byear.and.imonth.eq.bmonth)ibday=month_bday
      if(iyear.eq.eyear.and.imonth.eq.emonth)ieday=month_eday
      do iday=ibday,ieday ! loop over day
         jday=jday1+iday
         write(output_seed,'("seed/",i4.4,"_",i3.3,".seed")')iyear,jday
         inquire(file=output_seed,exist=ext)
!         write(*,*)'output_seed is',output_seed
         if (.not.ext)then
            write(command,'("touch",1x,1a)')trim(output_seed)
            call system(command)
            !write(*,'("jday=",i3.3)')jday
            !write(*,'("year=",i4.4,1x,"month=",i2.2,1x,"day=",i2.2)')iyear,imonth,iday
            write(year_day,'(i4.4,"_",i3.3)')iyear,jday
            write(seed,'("seed/",i4.4,"_",i3.3,".seed")')iyear,jday
            write(mdata,'("mdat/",i4.4,"_",i3.3,".mdata")')iyear,jday
            write(mail,'(i4.4,"_",i3.3,".info")')iyear,jday
            open(12,file=mail)
            write(12,'(".NAME",1x,1a5)')user
            write(12,'(".INST Macquarie University")')
            write(12,'(".MAIL Macquarie University, Sydney")')
            write(12,'(".EMAIL junxie01@gmail.com ")')
            write(12,'(".PHONE 8613721050718")')
            write(12,'(".FAX 5555513")')
            write(12,'(".MEDIA: Electronic (FTP) ")')
            write(12,'(".ALTERNATE MEDIA :  Electronic (FTP)")')
            write(12,'(".ALTERNATE MEDIA :  Electronic (FTP)")')
            write(12,'(".LABEL",1x,1a5)')label
            write(12,'(".QUALITY B")')
            write(12,'(".END")')
            write(12,'("")')
            do ista=1,n_sta
               do ic=1,icom
                  write(12,'(1a7,1x,1a2,1x,i4.4,1x,i2.2,1x,i2.2,1x,"00 00 00.000",&
                  1x,i4.4,1x,i2.2,1x,i2.2,1x,"23 59 59.999 1",1x,1a3)')stalist(ista),netlist(ista),&
                  iyear,imonth,iday,iyear,imonth,iday,com(ic)
               enddo
            enddo
            close(12)
            write(command,'("mkdir -p resp/resp_",i4.4,"_",i3.3,1x,"2>/dev/null")')iyear,jday
            call system(command)
            if (do_resp.eq.0)then
               write(command,'("FetchData -b",1x,1a,1x,"-o",1x,1a,1x,"-m&
               ",1x,1a,1x,"-rd resp/resp_",i4.4,"_",i3.3)')trim(mail),trim(output_seed),trim(mdata),iyear,jday
            else
               write(command,'("FetchData -b",1x,1a14,1x,"-rd resp/resp_",i4.4,"_",i3.3)')trim(mail),iyear,jday
            endif
            call system(command)
            write(command,'("rm",1x,1a)') mail 
            call system(command)
         endif ! if the seed does not exist
      enddo    ! end loop over day
   enddo       ! end loop over month
enddo          ! end loop over year
end program
