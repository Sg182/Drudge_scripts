
Module CCSDNp
    Use Precision
    Use Constants

    Contains

subroutine CCSD_Np(Np,U,V, T1, T2, z1, z2, NAO)

   use Precision
   implicit none
   integer, intent(in) :: NAO
   complex(kind=pr), intent(in) :: T1(NAO), T2(NAO,NAO)
   complex(kind=pr), intent(in) :: z1(NAO), z2(NAO,NAO)
   complex(kind=pr), intent(in) :: U(NAO), V(NAO)
   complex(kind=pr), intent(out) :: Np(NAO)

   integer                      :: p,q,r,s,i,j,k,l


    complex(kind=pr), dimension(:), allocatable :: tau0

    complex(kind=pr), dimension(:), allocatable :: tau1

    complex(kind=pr), dimension(:), allocatable :: tau2

    complex(kind=pr), dimension(:), allocatable :: tau3

    !$omp parallel default(shared)

    allocate(tau0(NAO))
    !$omp single
    tau0 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau0(p) = tau0(p) + ( &
                t2(p, q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau1(NAO))
    !$omp single
    tau1 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau1(p) = tau1(p) - ( &
            2 * t1(p) * tau0(p)&
        )
    
    end do
    !$omp end do

    allocate(tau2(NAO))
    !$omp single
    tau2 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau2(p) = tau2(p) + ( &
            tau0(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau0)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau1(p) = tau1(p) + ( &
            t1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau1(p) = tau1(p) + ( &
            z1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau1(p) = tau1(p) - ( &
            t1(p)**2 * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau1(p) = tau1(p) + ( &
                z1(q) * t2(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp single
    Np = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        Np(p) = Np(p) + ( &
            2 * tau1(p) * u(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau1)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau2(p) = tau2(p) + ( &
            t1(p) * z1(p)&
        )
    
    end do
    !$omp end do

    allocate(tau3(NAO))
    !$omp single
    tau3 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau3(p) = tau3(p) + ( &
            u(p)**2&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau3(p) = tau3(p) - ( &
            v(p)**2&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        Np(p) = Np(p) + ( &
            2 * tau2(p) * tau3(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau2)

    deallocate(tau3)

    !$omp do schedule(static)
    do p=1, NAO
    
        Np(p) = Np(p) + ( &
            2 * v(p)**2&
        )
    
    end do
    !$omp end do

    !$omp end parallel
    End Subroutine CCSD_Np
pure double precision function deltaf(p, q)
  implicit none
  integer, intent(in) :: p, q

  if (p == q) then
    deltaf = 1.0d0
  else
    deltaf = 0.0d0
  end if

end function deltaf
End Module CCSDNp