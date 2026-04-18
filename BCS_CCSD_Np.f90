
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


    complex(kind=pr), dimension(:), allocatable :: tau0_npnq

    complex(kind=pr), dimension(:), allocatable :: tau1_npnq

    complex(kind=pr), dimension(:), allocatable :: tau2_npnq

    complex(kind=pr), dimension(:), allocatable :: tau3_npnq

    !$omp parallel default(shared)

    allocate(tau0_npnq(NAO))
    !$omp single
    tau0_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau0_npnq(p) = tau0_npnq(p) + ( &
                t2(p, q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau1_npnq(NAO))
    !$omp single
    tau1_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau1_npnq(p) = tau1_npnq(p) - ( &
            2 * t1(p) * tau0_npnq(p)&
        )
    
    end do
    !$omp end do

    allocate(tau2_npnq(NAO))
    !$omp single
    tau2_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau2_npnq(p) = tau2_npnq(p) + ( &
            tau0_npnq(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau0_npnq)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau1_npnq(p) = tau1_npnq(p) + ( &
            t1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau1_npnq(p) = tau1_npnq(p) + ( &
            z1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau1_npnq(p) = tau1_npnq(p) - ( &
            t1(p)**2 * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau1_npnq(p) = tau1_npnq(p) + ( &
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
            2 * tau1_npnq(p) * u(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau1_npnq)

    !$omp do schedule(static)
    do p=1, NAO
    
        tau2_npnq(p) = tau2_npnq(p) + ( &
            t1(p) * z1(p)&
        )
    
    end do
    !$omp end do

    allocate(tau3_npnq(NAO))
    !$omp single
    tau3_npnq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau3_npnq(p) = tau3_npnq(p) + ( &
            u(p)**2&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        tau3_npnq(p) = tau3_npnq(p) - ( &
            v(p)**2&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
    
        Np(p) = Np(p) + ( &
            2 * tau2_npnq(p) * tau3_npnq(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau2_npnq)

    deallocate(tau3_npnq)

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