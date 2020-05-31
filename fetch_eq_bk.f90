! fetch seed data for earthquake data
program main
implicit none
integer nn,neqmax,nstmax
parameter (nn=21048576,neqmax=3000,nstmax=100)
integer i,j,nsta,id,ieq,nh
integer year,month,day,hour,mmin,sec,msec
integer eyear,emonth,eday,ehour,emmin,esec,emsec
integer imonth,nerr,nd,ic
integer do_resp,icom
integer n_sta,dsec,nm,ista
real stla,stlo,evlo,evla,evdp,mag
integer,dimension(12):: monday=(/31,28,31,30,31,30,31,31,30,31,30,31/)
character(10),dimension(1000) :: stalist,netlist
character(80)  :: args,user,label
character(80)  :: sta_list
character(3)   :: com(3),co
character(80)  :: chars,seed,mdata
character(100) :: mail,saclist,eqlist
character(180) :: command
character(180) :: para,file,output_seed
character(80)  :: dirout,bash,resp
logical ext
if(iargc().lt.1)then
   write(*,*)'Usage: fetch_data param.dat'
   write(*,*)'param.dat:'
   write(*,*)'station list: station, network, component'
   write(*,*)'earthquake list'
   write(*,*)'component icom dsec'
   write(*,*)'only download resp(1) or not(0)'
   call exit(-1)
endif
call getarg(1,para) ! read in control parameter
open(9,file=para)
read(9,'(a80)')sta_list
read(9,'(a80)')eqlist
read(9,*)co,icom,dsec
read(9,*)do_resp
close(9)
if(icom.eq.1)then
   com(1)=co
else
   com(1)=trim(co)//'Z'
   com(2)=trim(co)//'N'
   com(3)=trim(co)//'E'
endif

user="tmp"
label="tmp"
inquire(file=sta_list,exist=ext)
if(.not.ext)stop 'no station list found' ! read station list
open(13,file=sta_list)
do i=1,nstmax
   read(13,*,end=9,err=9)netlist(i),stalist(i)
enddo
9 close(13) 
n_sta=i-1 !number of station
write(*,*)'n_sta=',n_sta
inquire(file=eqlist,exist=ext)
if(.not.ext)stop 'earthquake list not exist'
call system("mkdir seed 2>/dev/null")
call system("mkdir resp 2>/dev/null")
call system("mkdir mdat 2>/dev/null")
open(15,file=eqlist)
do ieq=1,neqmax
   read(15,*,end=14,err=14)year,month,day,hour,mmin,sec,msec,evla,evlo,evdp,mag
   monday(2)=28
   if(mod(year,4)==0.and.mod(year,100).ne.0.or.mod(year,400).eq.0)monday(2)=29  
   eyear=year
   emonth=month
   eday=day
   emsec=msec
   nm=(dsec+sec)/60
   esec=mod((dsec+sec),60)
   nh=(mmin+nm)/60
   emmin=mod((mmin+nm),60)
   nd=(nh+hour)/24
   ehour=mod((nh+hour),24)
   if(nd.ge.1)eday=day+nd
   if(eday.gt.monday(month))then
      eday=1
      emonth=month+1
   endif
   if(emonth.gt.12)then
      emonth=1
      eyear=year+1
   endif
   write(output_seed,'("seed/",i4.4,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i3.3,".seed")')&
   year,month,day,hour,mmin,sec,msec
   inquire(file=output_seed,exist=ext)
!   if(ext)cycle
   write(command,'("touch",1x,1a)')trim(output_seed)
   call system(command)
   write(mdata,'("mdat/",i4.4,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i3.3,".mdata")')&
   year,month,day,hour,mmin,sec,msec
   write(mail,'(i4.4,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i2.2,"_",i3.3,".mail")')year,month,day,hour,mmin,sec,msec
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
   write(12,'(".LABEL",1x,1a5)')trim(label)
   write(12,'(".QUALITY B")')
   write(12,'(".END")')
   write(12,'("")')
   do ista=1,n_sta
      do ic=1,icom
         write(12,'(1a7,1x,1a2,1x,i4.4,1x,i2.2,1x,i2.2,1x,i2.2,1x,i2.2,1x,i2.2,".",i3.3,1x,&
         i4.4,1x,i2.2,1x,i2.2,1x,i2.2,1x,i2.2,1x,i2.2,".",i3.3," 1 ",1a3)')&
         stalist(ista),netlist(ista),year,month,day,hour,mmin,sec,msec,eyear,&
         emonth,eday,ehour,emmin,esec,emsec,com(ic)
      enddo
   enddo
   close(12)
   call system(command)
   if(do_resp.eq.0)then
      write(command,'("FetchData -b",1x,1a,1x,"-o",1x,1a,1x,"-m",1x,1a,1x,"-rd resp")')&
      trim(mail),trim(output_seed),trim(mdata)
   else
      write(command,'("FetchData -b",1x,1a,1x,"-rd resp")')trim(mail)
      write(*,'("FetchData -b",1x,1a,1x,"-rd resp")')trim(mail)
   endif
   call system(command)
   write(command,'("rm",1x,1a)') mail 
   call system(command)
enddo ! loop over year
14 close(15)
!write(*,'("monday=",12i10)')(monday(i),i=1,12)
end program
