

c/////////////////////////////////////////////////////////
c     x is the initial counter of the list
c     avg is referencing the mean
c     r is referencing the range of list min to high
c     s_dev is the population standard deviation using(n-1)
c     p references p90
c     p1 references p95
c     p2 references p99
c     i refernces IQR
c     quart1 is the 1st Quartile
c     quart2 is the 2nd Quartile
c     quart3 is the 3rd Quartile
c/////////////////////////////////////////////////////////
      program project1
      real x (500000)
      integer n
      real summation
      real mean
      real median
      real mode
      real r_mintohigh
      real mabdev
      real standev
      real covar
      real vari
      real quart
      real pninezero
      real pninefiv
      real iqr
      character filepath*(128)
      sumation=0.0
      n=0
      print *, "Path to file:"
      read(*,*)filepath
      open(unit=1,file=filepath)
c loop through the file until end
      do
        read(1,*,end=10)x(n+1)
        n=n+1
      end do
 10   close(1)
c count, n, will be off by one
      do i=1,n
        write(*,*)x(i)
      end do





c/////////////////////////////////////////////////////////
c Variables for functions
      avg = mean(x,n)
      r = r_mintohigh(x,n)
      s_dev = standev(x,n,avg)
      p = pninezero(x,n)
      p1 = pninefive(x,n)
      p2 = pninenine(x,n)
c/////////////////////////////////////////////////////////

c/////////////////////////////////////////////////////////
c Print statements for calc functions

      print *, "Mean--------------->","The Answer is:", mean(x,n)
      print *, "Median------------->","The Answer is:", median(x,n)
      print *, "Mode---------------> ","The Answer is:", mode(x,n)
      print *, "Range--------------> ","The Answer is:", r
      print *, "Mean Abs Deviation->","The Answer is:",mabdev(x,n,avg)
      print *, "Standard Deviation-> ","The Answer is:",s_dev
      print *, "COV is------------->","The Answer is:", covar(s_dev,avg)
      print *, "Variance-----------> ","The Answer is:",vari(s_dev)
      q = quart(x,n)
      print *, "p90----------------> ","The Answer is:",p
      print *, "p95----------------> ","The Answer is:",p1
      print *, "p99----------------> ","The Answer is:",p2
      i= iqr(x,n)
      end
c/////////////////////////////////////////////////////////
c//////////Functions for Statistical Values///////////////


c////////// Function to calculate mean///////////////////

      real function mean(list,num)
      real list(num)
      integer num
      listtotal = sum(list)
      mean = listtotal / num
      return
      end


c///////// Function to calculate median/////////////////
      real function median(list,num)
      real list(num)
      integer num
      if (mod(num,2) .ne. 0) then
          median = real(list(num/2 +1))
      else
           median = real(list(num/2)+list(num/2 + 1))/2
      end if
      return
      end


c//////////// Function to calculate Mode///////////////////
c  The "mode" is the value that occurs most often. If no number in the
c  list is repeated, then there is no mode for the list.
c/////////////////////////////////////////////////////////
      real function mode(list,num)
      real list(num),z1,z2
      integer num, count1, count2
      count1=1
      count2=0
      z1=1
      z2=0

      mode=list(1)
      do 100 j=1,num
      if(mode.eq.list(j+1))then
        count1=count1+1
        end if
      if(count1.ne.1)then
        z1=list(j)
      else if((z1.eq.mode).and.(z1.eq.list(j+1)))then
        count1=cout1+1
      else if((z1.eq.mode).and.(z1.eq.list(j+1)))then
        count2=cout2+1
      elseif((z1.ne.z2).and.(z1.ne.mode).and.(z1.eq.list(j+1)))then
        count2=1
        z1=z2
      else if (count2.gt.count1)then
        mode=z2
        count1=count2
        z2=0
        count2=0
        end if
 100  continue
      return
      end



c///////////////// Function to calculate Range//////////////////////
c  "The range of a set of data is the difference between the highest and
c  lowest values in the set. To find the range, first order the data from
c  least to greatest. Then subtract the smallest value from the largest
c  value in the set.
c///////////////////////////////////////////////////////////////////
      real function r_mintohigh(list,num)
      real list(num)
      integer num
      range = list(num) - list(1)
      return
      end


c////////// Function to calculate mean abs deviation/////////////////
c   To find the mean absolute deviation of the data, start by finding the
c   mean of the data set.
c
c  Find the sum of the data values, and divide the sum by the number of
c    data values.

c  Find the absolute value of the difference between each data value and
c    the mean: ...

c  Find the sum of the absolute values of the differences.

c   Divide the sum of the absolute values of the differences by the
c     number of data values.
c////////////////////////////////////////////////////////////////////

      real function mabdev(list,num,avg)
      real list(num), sumdiff, avg
      integer num
      sumdiff = 0.0
      do 50, i=1,num
        sumdiff = sumdiff + abs((avg - list(i)))
 50     continue
      mabdev = (sumdiff/num)
      return
      end

c/////////// Function to calculate Stand Deviation/////////////////

c   Work out the Mean (the simple average of the numbers)

c   Then for each number: subtract the Mean and square the result.

c   Then work out the mean of those squared differences.

c    Take the square root of that and we are done!

c""""""""the standard deviation that I used is (n-1)""""""
c"""""""which is the population equation"""""""""
c//////////////////////////////////////////////////////////////////
      real function standev(list,num,m)
      real list(num), m, sumsqu
      integer num
      sumsqu = 0.0
      do 150, i = 1,num
        sumsqu = sumsqu + (list(i) - m)**2
 150  continue
      stdnev = sqrt(sumsqu / (num - 1))
      end



c/////// Function to calculate Coefficient of Variation///////////
c Coefficient of Variation = (Standard Deviation / Mean)
c In symbols: CV = (SD/xbar)
c///////////////////////////////////////////////////////////////
      real function covar(standev,m)
      real standev, m
      covar = (standev/m)
      return
      end

c////////////// Function to calculate variance////////////////
c   Variance is a measure of how spread out a data set is. It useful when
c   c reating statistical models, since low variance can be a sign that you
c   are over-fitting your data.
c////////////////////////////////////////////////////////////
      real function vari(standev)
      real standev
      vari = standev**2
      return
      end
c//////////// Function to calculate Quartiles/////////////////
c   Use the median to divide the ordered data set into two halves. If
c  there are an odd number of data points in the original ordered data set,
c  do not include the median (the central value in the ordered list) in
c  either half. ...
c  The lower quartile value is the median of the lower half of the data.
c///////////////////////////////////////////////////////////////
      real function quart(list,num)
      real list(num), quart1, quart2, quart3, median,iqr
      integer num
      quart2 = median(list,num)
      quart1 = median(list,num/2)
      quart3 = median(list(num/2+1:num),num/2)
      iqr= ((quart3)-(quart1))
      print *,"Q1 is--------------->","The Answer is:", quart1
      print *,"Q2 is--------------->","The Answer is:", quart2
      print *,"Q3 is--------------->","The Answer is:", quart3
      print *,"IQR is-------------->","The Answer is:",iqr
      return
      end




c////////////////////////////////////////////////////////////////
c the following three functions calculate the p values
c Function for calculating p90
      real function pninezero (list,num)
      real list(num)
      integer num, num1
      num1 =num*.90
      pninezero =list(num1)
      return
      end

c Function to calculate p95
      real function pninefive(list,num)
      real list (num)
      integer num,num2
      num2 =num*.95
      pninefive =list(num2)
      return
      end

c Function to calculate p99
      real function pninenine(list,num)
      real list (num)
      integer num,num3
      num3 =num*.99
      pninenine =list(num3)
      return
      end
c///////////////////////////////////////////////////////////////

c//////////////// Function for finding IQR/////////////////////
c Find the median of the lower and upper half of your data. The median is
c the "midpoint," or the number that is halfway into a set. ...
c Subtract Q3 - Q1 to determine the IQR. Now you know how many numbers lie
c between the 25th percentile and the 75th percentile.
c///////////////////////////////////////////////////////////////
      real function iqr (list,num)
      real list (num),quart3,quart1

      return
      end
c Function for bins
c      real function bins(list,num)
c      real list(num)
c      integer num,i
c      if (i <= 10)then
c      write(*,*)," Extra, Credit"
c      read (*,*) num
c      write(*,*) 'Enter the number of bins',i
