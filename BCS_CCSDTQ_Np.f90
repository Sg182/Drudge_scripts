!MODULE FOR Np EXPECTATION
Module CCSDTQNp
    Use Precision
    Use Constants

    Contains

subroutine CCSDTQ_Np(Np,U,V, T1,T2,T3,T4, z1, z2,z3,z4, NAO)

   use Precision
   implicit none
   integer, intent(in) :: NAO
   complex(kind=pr), intent(in) :: T1(NAO), T2(NAO,NAO)
   complex(kind=pr), intent(in) :: T3(NAO,NAO,NAO), T4(NAO,NAO,NAO,NAO)
   complex(kind=pr), intent(in) :: z1(NAO), z2(NAO,NAO)
   complex(kind=pr), intent(in) :: U(NAO), V(NAO)
   complex(kind=pr), intent(out) :: Np(NAO)

   integer                      :: p,q,r,s,i,j,k,l

 

    complex(kind=pr), dimension(na) :: tau0_np
    complex(kind=pr), dimension(na, na, na, na) :: tau1_np
    complex(kind=pr), dimension(na, na, na) :: tau2_np
    complex(kind=pr), dimension(na, na) :: tau3_np
    complex(kind=pr), dimension(na) :: tau4_np
    complex(kind=pr), dimension(na) :: tau5_np
    complex(kind=pr), dimension(na) :: tau6_np
    complex(kind=pr), dimension(na) :: tau7_np
     


    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, na
        tau0_np(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        tau0_np(p) = tau0_np(p) + ( &
            v(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        tau0_np(p) = tau0_np(p) - ( &
            u(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        tau0_np(p) = tau0_np(p) + ( &
            2 * t1(p) * u(p) * v(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, na
        do q=1, na
            do r=1, na
                do s=1, na
                    tau1_np(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        do q=1, na
            do r=1, na
                do s=1, na
                    tau1_np(p, q, r, s) = tau1_np(p, q, r, s) + ( &
                        6 * u(p) * v(p) * t2(r, p) * t3(q, p, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        do q=1, na
            do r=1, na
                do s=1, na
                    tau1_np(p, q, r, s) = tau1_np(p, q, r, s) + ( &
                        tau0_np(p) * t4(s, p, q, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, na
        do q=1, na
            do r=1, na
                tau2_np(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        do q=1, na
            do r=1, na
                tau2_np(p, q, r) = tau2_np(p, q, r) + ( &
                    2 * u(p) * v(p) * t2(q, p) * t2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        do q=1, na
            do r=1, na
                tau2_np(p, q, r) = tau2_np(p, q, r) + ( &
                    tau0_np(p) * t3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, na
        do q=1, na
            tau3_np(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        do q=1, na
            tau3_np(p, q) = tau3_np(p, q) - ( &
                u(p) * v(p) * z1(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        do q=1, na
            tau3_np(p, q) = tau3_np(p, q) + ( &
                tau0_np(p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, na
        tau4_np(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        do q=1, na
            do r=1, na
                tau4_np(p) = tau4_np(p) + ( &
                    z2(r, q) * t3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, na
        tau5_np(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        do q=1, na
            do r=1, na
                do s=1, na
                    tau5_np(p) = tau5_np(p) + ( &
                        z3(s, q, r) * t4(s, p, q, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, na
        tau6_np(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        tau6_np(p) = tau6_np(p) + ( &
            6 * t1(p) * u(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        tau6_np(p) = tau6_np(p) + ( &
            6 * u(p) * z1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        tau6_np(p) = tau6_np(p) - ( &
            6 * u(p) * z1(p) * t1(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        tau6_np(p) = tau6_np(p) + ( &
            3 * tau4_np(p) * u(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        tau6_np(p) = tau6_np(p) + ( &
            tau5_np(p) * u(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, na
        tau7_np(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        tau7_np(p) = tau7_np(p) + ( &
            u(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        tau7_np(p) = tau7_np(p) - ( &
            v(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, na
        Np(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        Np(p) = Np(p) + ( &
            2 * v(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        do q=1, na
            do r=1, na
                do s=1, na
                    Np(p) = Np(p) - ( &
                        tau1_np(p, q, r, s) * z4(q, p, r, s) / 3 &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        do q=1, na
            do r=1, na
                Np(p) = Np(p) - ( &
                    tau2_np(p, q, r) * z3(q, p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        do q=1, na
            Np(p) = Np(p) - ( &
                2 * t2(q, p) * tau3_np(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        Np(p) = Np(p) + ( &
            tau6_np(p) * v(p) / 3 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, na
        Np(p) = Np(p) + ( &
            2 * t1(p) * tau7_np(p) * z1(p) &
        )
    end do
    !$omp end do


    !$omp end parallel
        End Subroutine CCSDTQ_Np
pure double precision function deltaf(p, q)
  implicit none
  integer, intent(in) :: p, q

  if (p == q) then
    deltaf = 1.0d0
  else
    deltaf = 0.0d0
  end if

end function deltaf
End Module CCSDTQNp
