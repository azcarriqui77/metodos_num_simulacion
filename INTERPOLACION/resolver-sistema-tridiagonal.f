      program pru
      implicit none
      integer n,nmax,i
      parameter(nmax=100)
      real a(nmax),b(nmax-1),c(nmax-1),v(nmax),z(nmax)
c sistema tridiagonal de prueba para resolver
      n= 4
      a(n)=n
      do i=1,n-1
         a(i)=i
         b(i)=1
         c(i)=1
      enddo
      v(1)=2
      do i=2,n-1
         v(i) = i+2
      enddo
      v(n)=n+1
      write(*,*)'b(i) ',(b(i),i=1,n-1)
      write(*,*)'a(i) ', (a(i),i=1,n)
      write(*,*)'c(i) ', (c(i),i=1,n-1)
      write(*,*)'v(i) ',(v(i),i=1,n)
      call solvetrid(nmax,n,a,b,c,v,z)
      write(*,*)'z(i) ',(z(i),i=1,n)
      pause
      stop
      end
c-------------------
      subroutine solvetrid(nmax,n,a,b,c,v,z)
c---------------------------------
c     ! resuelve sistema tridiagonal
c     ! M = 
c     ! ( a1 b1 0   ...            0  )
c     ! ( c1 a2 b2  ...            0  )
c     ! ( 0  c2 a3 b3 ...          0  )
c     ! (       ......                )
c     ! ( ........c(n-2) a(n-1) b(n-1)) 
c     ! ( 0......    0   c(n-1) a(n)  )
c     ! M . z = v  
c     !    donde v es el vector columna de v(1),...v(n)
c     !    donde z es el vector columna de z(1),...z(n)
c datos v(i) son terminos independientes del sistema lineal de ecuaciones
c      ! Los resultados z(i) son las incognitas del sistema 
      implicit none
      integer n,nmax,i
      real a(nmax),b(nmax-1),c(nmax-1),v(nmax),z(nmax),factor
c!transformamos el sistema en un triangular superior equivalente
c! restando a cada fila la fila anterio por el peso adecuado para 
c! eliminar el termino con ck y resulta la matriz extendida
c! del tipo siguiente donde se han redefinido las a(i) y las v(i)
c      ! M = 
c      ! ( a1 b1 0   ...            0  ) (v1)
c      ! (  0 a2 b2  ...            0  ) (v2)
c      ! ( 0   0 a3 b3 ...          0  ) (v3)
c      ! (       ......                )  ...
c      ! ( ........   0  a(n-1)  b(n-1)) 
c      ! ( 0......    0    0     a(n)  ) (v(n))
c      ! M . z = v  
c!
      do i=2,n
         factor = c(i-1)/a(i-1)
         a(i) = a(i)- factor*b(i-1)
         v(i) = v(i)- factor*v(i-1)
      enddo
c se resuelve de abajo hacia arriba
      z(n) = v(n)/a(n)
      do i=n-1,1,-1
         z(i)= (v(i)-b(i)*z(i+1))/a(i)
      enddo
      end
c---------------------------------
