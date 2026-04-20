Module CCSDTQSpSq
    Use Precision
    Use Constants

    Contains

subroutine CCSDTQ_SpSq(SpSq, T1,T2,T3,T4, z1, z2,z3,z4, NAO, &
     S00, S20p, S20q, S11p, S11q, S02p, S02q, &
     S40, S31pq, S31qp, S22NN, ST22pq, ST22qp, S13pq, S13qp, S04)

   use Precision
   implicit none
   integer, intent(in) :: NAO
   complex(kind=pr), intent(in) :: T1(NAO), T2(NAO,NAO), T3(NAO,NAO,NAO), T4(NAO,NAO,NAO,NAO)
   complex(kind=pr), intent(in) :: z1(NAO), z2(NAO,NAO), z3(NAO,NAO,NAO), z4(NAO,NAO,NAO,NAO)

   complex(kind=pr), intent(in) :: S00(NAO,NAO)
   complex(kind=pr), intent(in) :: S20p(NAO,NAO), S20q(NAO,NAO)
   complex(kind=pr), intent(in) :: S11p(NAO,NAO), S11q(NAO,NAO)
   complex(kind=pr), intent(in) :: S02p(NAO,NAO), S02q(NAO,NAO)
   complex(kind=pr), intent(in) :: S40(NAO,NAO), S04(NAO,NAO)
   complex(kind=pr), intent(in) :: S31pq(NAO,NAO), S31qp(NAO,NAO)
   complex(kind=pr), intent(in) :: S22NN(NAO,NAO)
   complex(kind=pr), intent(in) :: ST22pq(NAO,NAO), ST22qp(NAO,NAO)
   complex(kind=pr), intent(in) :: S13pq(NAO,NAO), S13qp(NAO,NAO)
   integer                      :: p,q,r,s,i,j,k,l,p0
   complex(kind=pr), intent(out) :: SpSq(NAO,NAO)
   complex(kind=pr) :: tmp


    complex(kind=pr) , dimension(:, :), allocatable :: tau0_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau1_spsq

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau2_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau3_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau4_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau5_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau6_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau7_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau8_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau9_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau10_spsq

    complex(kind=pr) , dimension(:, :, :, :), allocatable :: tau11_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau12_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau13_spsq

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau14_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau15_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau16_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau17_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau18_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau19_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau20_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau21_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau22_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau23_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau24_spsq

    complex(kind=pr) , dimension(:, :, :), allocatable :: tau25_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau26_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau27_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau28_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau29_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau30_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau31_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau32_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau33_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau34_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau35_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau36_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau37_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau38_spsq

    complex(kind=pr) , dimension(:, :, :, :), allocatable :: tau39_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau40_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau41_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau42_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau43_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau44_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau45_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau46_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau47_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau48_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau49_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau50_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau51_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau52_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau53_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau54_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau55_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau56_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau57_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau58_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau59_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau60_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau61_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau62_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau63_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau64_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau65_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau66_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau67_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau68_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau69_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau70_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau71_spsq

    complex(kind=pr) , dimension(:), allocatable :: tau72_spsq

    complex(kind=pr) , dimension(:, :), allocatable :: tau73_spsq


    allocate(tau0_spsq(NAO, NAO))
    tau0_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do p0=1, NAO
                do s=1, NAO
                    do r=1, NAO
                        tau0_spsq(p, q) = tau0_spsq(p, q) + ( &
                            t4(p, r, s, p0) * z4(q, r, s, p0)&
                        )
                    end do
                end do
            end do
        end do
    end do

    allocate(tau19_spsq(NAO, NAO))
    tau19_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau19_spsq(p, q) = tau19_spsq(p, q) + ( &
                tau0_spsq(p, q)&
            )
    
        end do
    end do

    allocate(tau24_spsq(NAO, NAO))
    tau24_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau24_spsq(p, q) = tau24_spsq(p, q) + ( &
                t1(p)**2 * tau0_spsq(q, p)&
            )
    
        end do
    end do

    allocate(tau49_spsq(NAO, NAO))
    tau49_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau49_spsq(p, q) = tau49_spsq(p, q) - ( &
                tau0_spsq(p, q)&
            )
    
        end do
    end do

    allocate(tau57_spsq(NAO, NAO))
    tau57_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau57_spsq(p, q) = tau57_spsq(p, q) + ( &
                tau0_spsq(p, q)&
            )
    
        end do
    end do

    allocate(tau59_spsq(NAO, NAO))
    tau59_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau59_spsq(p, q) = tau59_spsq(p, q) + ( &
                tau0_spsq(p, q)&
            )
    
        end do
    end do

    SpSq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                ST22qp(p, q) * tau0_spsq(p, q) / 6&
            )
    
        end do
    end do

    deallocate(tau0_spsq)

    allocate(tau1_spsq(NAO, NAO))
    tau1_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do s=1, NAO
                do r=1, NAO
                    tau1_spsq(p, q) = tau1_spsq(p, q) + ( &
                        t4(p, r, s, q) * z4(p, r, s, q)&
                    )
                end do
            end do
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * S22NN(p, q) * tau1_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau1_spsq)

    allocate(tau2_spsq(NAO, NAO, NAO))
    tau2_spsq = 0.0

    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau2_spsq(p, q, r) = tau2_spsq(p, q, r) + ( &
                        t2(p, s) * z4(p, s, r, q)&
                    )
                end do
            end do
        end do
    end do

    allocate(tau3_spsq(NAO, NAO))
    tau3_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau3_spsq(p, q) = tau3_spsq(p, q) + ( &
                    t2(p, r) * tau2_spsq(q, p, r)&
                )
            end do
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                4 * S22NN(p, q) * tau3_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau3_spsq)

    allocate(tau16_spsq(NAO, NAO))
    tau16_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do s=1, NAO
                do r=1, NAO
                    tau16_spsq(p, q) = tau16_spsq(p, q) + ( &
                        t3(q, r, s) * tau2_spsq(p, s, r)&
                    )
                end do
            end do
        end do
    end do

    allocate(tau18_spsq(NAO, NAO))
    tau18_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau18_spsq(p, q) = tau18_spsq(p, q) + ( &
                3 * tau16_spsq(p, q)&
            )
    
        end do
    end do

    allocate(tau52_spsq(NAO, NAO))
    tau52_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau52_spsq(p, q) = tau52_spsq(p, q) - ( &
                3 * tau16_spsq(p, q)&
            )
    
        end do
    end do

    allocate(tau58_spsq(NAO, NAO))
    tau58_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau58_spsq(p, q) = tau58_spsq(p, q) + ( &
                3 * tau16_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau16_spsq)

    allocate(tau4_spsq(NAO, NAO))
    tau4_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do s=1, NAO
                do r=1, NAO
                    tau4_spsq(p, q) = tau4_spsq(p, q) + ( &
                        t3(p, r, s) * z3(s, q, r)&
                    )
                end do
            end do
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau19_spsq(p, q) = tau19_spsq(p, q) + ( &
                3 * tau4_spsq(p, q)&
            )
    
        end do
    end do

    allocate(tau20_spsq(NAO, NAO))
    tau20_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau20_spsq(p, q) = tau20_spsq(p, q) + ( &
                    t2(r, p) * tau19_spsq(q, r)&
                )
            end do
        end do
    end do

    deallocate(tau19_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau24_spsq(p, q) = tau24_spsq(p, q) - ( &
                tau20_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau20_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau49_spsq(p, q) = tau49_spsq(p, q) - ( &
                3 * tau4_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau57_spsq(p, q) = tau57_spsq(p, q) + ( &
                3 * tau4_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau59_spsq(p, q) = tau59_spsq(p, q) + ( &
                3 * tau4_spsq(p, q)&
            )
    
        end do
    end do

    allocate(tau61_spsq(NAO, NAO))
    tau61_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau61_spsq(p, q) = tau61_spsq(p, q) - ( &
                tau4_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                t1(p)**2 * S40(q, p) * tau4_spsq(q, p) / 2&
            )
    
        end do
    end do

    deallocate(tau4_spsq)

    allocate(tau5_spsq(NAO, NAO))
    tau5_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do s=1, NAO
                do r=1, NAO
                    tau5_spsq(p, q) = tau5_spsq(p, q) + ( &
                        t3(p, r, s) * z4(p, q, r, s)&
                    )
                end do
            end do
        end do
    end do

    allocate(tau10_spsq(NAO, NAO))
    tau10_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau10_spsq(p, q) = tau10_spsq(p, q) + ( &
                    tau5_spsq(p, r) * t3(p, r, q)&
                )
            end do
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau24_spsq(p, q) = tau24_spsq(p, q) + ( &
                6 * tau10_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau10_spsq)

    allocate(tau17_spsq(NAO, NAO))
    tau17_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau17_spsq(p, q) = tau17_spsq(p, q) + ( &
                    t2(p, r) * tau5_spsq(q, r)&
                )
            end do
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau18_spsq(p, q) = tau18_spsq(p, q) + ( &
                3 * tau17_spsq(q, p)&
            )
    
        end do
    end do

    deallocate(tau17_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau18_spsq(p, q) = tau18_spsq(p, q) - ( &
                6 * t2(p, q) * tau5_spsq(q, p)&
            )
    
        end do
    end do

    allocate(tau22_spsq(NAO))
    tau22_spsq = 0.0

    do p=1, NAO
        do q=1, NAO
            tau22_spsq(p) = tau22_spsq(p) + ( &
                t2(p, q) * tau5_spsq(p, q)&
            )
        end do
    end do

    allocate(tau23_spsq(NAO))
    tau23_spsq = 0.0

    do p=1, NAO
    
        tau23_spsq(p) = tau23_spsq(p) + ( &
            6 * tau22_spsq(p)&
        )
    
    end do

    deallocate(tau22_spsq)

    allocate(tau47_spsq(NAO, NAO))
    tau47_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau47_spsq(p, q) = tau47_spsq(p, q) + ( &
                tau5_spsq(p, q)&
            )
    
        end do
    end do

    allocate(tau48_spsq(NAO, NAO))
    tau48_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau48_spsq(p, q) = tau48_spsq(p, q) + ( &
                tau5_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau61_spsq(p, q) = tau61_spsq(p, q) + ( &
                2 * t1(p) * tau5_spsq(p, q)&
            )
    
        end do
    end do

    allocate(tau64_spsq(NAO, NAO))
    tau64_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau64_spsq(p, q) = tau64_spsq(p, q) + ( &
                tau5_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                t1(p)**2 * t1(q) * S40(q, p) * tau5_spsq(q, p)&
            )
    
        end do
    end do

    allocate(tau6_spsq(NAO, NAO))
    tau6_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do s=1, NAO
                do r=1, NAO
                    tau6_spsq(p, q) = tau6_spsq(p, q) + ( &
                        t2(p, r) * t2(p, s) * z4(p, r, s, q)&
                    )
                end do
            end do
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau49_spsq(p, q) = tau49_spsq(p, q) + ( &
                6 * tau6_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau57_spsq(p, q) = tau57_spsq(p, q) - ( &
                6 * tau6_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau59_spsq(p, q) = tau59_spsq(p, q) - ( &
                6 * tau6_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau61_spsq(p, q) = tau61_spsq(p, q) + ( &
                2 * tau6_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                t1(p)**2 * S40(q, p) * tau6_spsq(q, p)&
            )
    
        end do
    end do

    deallocate(tau6_spsq)

    allocate(tau7_spsq(NAO, NAO))
    tau7_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau7_spsq(p, q) = tau7_spsq(p, q) + ( &
                    t2(p, r) * z2(q, r)&
                )
            end do
        end do
    end do

    allocate(tau38_spsq(NAO, NAO))
    tau38_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau38_spsq(p, q) = tau38_spsq(p, q) + ( &
                2 * t1(p)**2 * tau7_spsq(q, p)&
            )
    
        end do
    end do

    allocate(tau46_spsq(NAO, NAO))
    tau46_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau46_spsq(p, q) = tau46_spsq(p, q) + ( &
                    4 * t2(q, r) * tau7_spsq(p, r)&
                )
            end do
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau49_spsq(p, q) = tau49_spsq(p, q) - ( &
                6 * tau7_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau57_spsq(p, q) = tau57_spsq(p, q) + ( &
                6 * tau7_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau59_spsq(p, q) = tau59_spsq(p, q) + ( &
                6 * tau7_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                ST22qp(p, q) * tau7_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau7_spsq)

    allocate(tau8_spsq(NAO, NAO))
    tau8_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau8_spsq(p, q) = tau8_spsq(p, q) + ( &
                    t3(p, r, q) * z3(p, r, q)&
                )
            end do
        end do
    end do

    allocate(tau41_spsq(NAO, NAO))
    tau41_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau41_spsq(p, q) = tau41_spsq(p, q) + ( &
                2 * tau8_spsq(q, p)&
            )
    
        end do
    end do

    allocate(tau42_spsq(NAO, NAO))
    tau42_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau42_spsq(p, q) = tau42_spsq(p, q) + ( &
                2 * tau8_spsq(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                4 * S22NN(p, q) * tau8_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau8_spsq)

    allocate(tau9_spsq(NAO))
    tau9_spsq = 0.0

    do p=1, NAO
        do s=1, NAO
            do r=1, NAO
                do q=1, NAO
                    tau9_spsq(p) = tau9_spsq(p) + ( &
                        t4(p, q, r, s) * z4(p, q, r, s)&
                    )
                end do
            end do
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau18_spsq(p, q) = tau18_spsq(p, q) + ( &
                t1(q) * tau9_spsq(p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau24_spsq(p, q) = tau24_spsq(p, q) + ( &
                2 * tau9_spsq(p) * t2(p, q)&
            )
    
        end do
    end do

    allocate(tau51_spsq(NAO))
    tau51_spsq = 0.0

    do p=1, NAO
    
        tau51_spsq(p) = tau51_spsq(p) + ( &
            tau9_spsq(p)&
        )
    
    end do

    allocate(tau70_spsq(NAO))
    tau70_spsq = 0.0

    do p=1, NAO
    
        tau70_spsq(p) = tau70_spsq(p) + ( &
            tau9_spsq(p)&
        )
    
    end do

    deallocate(tau9_spsq)

    allocate(tau11_spsq(NAO, NAO, NAO, NAO))
    tau11_spsq = 0.0

    do s=1, NAO
        do r=1, NAO
            do q=1, NAO
                do p=1, NAO
    
                    tau11_spsq(p, q, r, s) = tau11_spsq(p, q, r, s) + ( &
                        t4(s, p, q, r)&
                    )
    
                end do
            end do
        end do
    end do

    do s=1, NAO
        do r=1, NAO
            do q=1, NAO
                do p=1, NAO
    
                    tau11_spsq(p, q, r, s) = tau11_spsq(p, q, r, s) + ( &
                        t2(p, r) * t2(s, q)&
                    )
    
                end do
            end do
        end do
    end do

    allocate(tau12_spsq(NAO, NAO))
    tau12_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do s=1, NAO
                do r=1, NAO
                    tau12_spsq(p, q) = tau12_spsq(p, q) + ( &
                        tau2_spsq(p, r, s) * tau11_spsq(r, p, q, s)&
                    )
                end do
            end do
        end do
    end do

    deallocate(tau2_spsq)

    deallocate(tau11_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau24_spsq(p, q) = tau24_spsq(p, q) + ( &
                6 * tau12_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau12_spsq)

    allocate(tau13_spsq(NAO, NAO))
    tau13_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do s=1, NAO
                do r=1, NAO
                    tau13_spsq(p, q) = tau13_spsq(p, q) + ( &
                        z3(p, r, s) * t4(p, r, s, q)&
                    )
                end do
            end do
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau18_spsq(p, q) = tau18_spsq(p, q) + ( &
                3 * tau13_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau52_spsq(p, q) = tau52_spsq(p, q) - ( &
                3 * tau13_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau58_spsq(p, q) = tau58_spsq(p, q) + ( &
                3 * tau13_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau13_spsq)

    allocate(tau14_spsq(NAO, NAO, NAO))
    tau14_spsq = 0.0

    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do s=1, NAO
                    tau14_spsq(p, q, r) = tau14_spsq(p, q, r) + ( &
                        t3(p, s, q) * z4(p, r, s, q)&
                    )
                end do
            end do
        end do
    end do

    allocate(tau15_spsq(NAO, NAO))
    tau15_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau15_spsq(p, q) = tau15_spsq(p, q) + ( &
                    t2(p, r) * tau14_spsq(q, p, r)&
                )
            end do
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau18_spsq(p, q) = tau18_spsq(p, q) - ( &
                12 * tau15_spsq(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau24_spsq(p, q) = tau24_spsq(p, q) + ( &
                2 * t1(p) * tau18_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau18_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau52_spsq(p, q) = tau52_spsq(p, q) + ( &
                12 * tau15_spsq(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau58_spsq(p, q) = tau58_spsq(p, q) - ( &
                12 * tau15_spsq(q, p)&
            )
    
        end do
    end do

    deallocate(tau15_spsq)

    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau46_spsq(p, q) = tau46_spsq(p, q) + ( &
                    8 * t3(q, r, p) * tau14_spsq(q, p, r)&
                )
            end do
        end do
    end do

    deallocate(tau14_spsq)

    allocate(tau21_spsq(NAO))
    tau21_spsq = 0.0

    do p=1, NAO
        do s=1, NAO
            do r=1, NAO
                do q=1, NAO
                    tau21_spsq(p) = tau21_spsq(p) + ( &
                        z3(s, q, r) * t4(p, q, r, s)&
                    )
                end do
            end do
        end do
    end do

    do p=1, NAO
    
        tau23_spsq(p) = tau23_spsq(p) - ( &
            tau21_spsq(p)&
        )
    
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau24_spsq(p, q) = tau24_spsq(p, q) + ( &
                t1(p) * tau23_spsq(q)&
            )
    
        end do
    end do

    deallocate(tau23_spsq)

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                S40(q, p) * tau24_spsq(p, q) / 6&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                S40(p, q) * tau24_spsq(q, p) / 6&
            )
    
        end do
    end do

    deallocate(tau24_spsq)

    allocate(tau69_spsq(NAO))
    tau69_spsq = 0.0

    do p=1, NAO
    
        tau69_spsq(p) = tau69_spsq(p) - ( &
            tau21_spsq(p)&
        )
    
    end do

    allocate(tau72_spsq(NAO))
    tau72_spsq = 0.0

    do p=1, NAO
    
        tau72_spsq(p) = tau72_spsq(p) + ( &
            tau21_spsq(p)&
        )
    
    end do

    deallocate(tau21_spsq)

    allocate(tau25_spsq(NAO, NAO, NAO))
    tau25_spsq = 0.0

    do r=1, NAO
        do q=1, NAO
            do p=1, NAO
                do p0=1, NAO
                    do s=1, NAO
                        tau25_spsq(p, q, r) = tau25_spsq(p, q, r) + ( &
                            t3(p, s, p0) * z4(q, r, s, p0)&
                        )
                    end do
                end do
            end do
        end do
    end do

    do q=1, NAO
        do p=1, NAO
            do s=1, NAO
                do r=1, NAO
                    tau46_spsq(p, q) = tau46_spsq(p, q) + ( &
                        t3(q, r, s) * tau25_spsq(p, s, r)&
                    )
                end do
            end do
        end do
    end do

    deallocate(tau25_spsq)

    allocate(tau26_spsq(NAO))
    tau26_spsq = 0.0

    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau26_spsq(p) = tau26_spsq(p) + ( &
                    t3(p, q, r) * z3(p, q, r)&
                )
            end do
        end do
    end do

    allocate(tau32_spsq(NAO))
    tau32_spsq = 0.0

    do p=1, NAO
    
        tau32_spsq(p) = tau32_spsq(p) + ( &
            tau26_spsq(p)&
        )
    
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau38_spsq(p, q) = tau38_spsq(p, q) + ( &
                2 * tau26_spsq(p) * t2(p, q)&
            )
    
        end do
    end do

    do p=1, NAO
    
        tau51_spsq(p) = tau51_spsq(p) + ( &
            3 * tau26_spsq(p)&
        )
    
    end do

    do p=1, NAO
    
        tau70_spsq(p) = tau70_spsq(p) + ( &
            3 * tau26_spsq(p)&
        )
    
    end do

    deallocate(tau26_spsq)

    allocate(tau27_spsq(NAO, NAO))
    tau27_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau27_spsq(p, q) = tau27_spsq(p, q) + ( &
                    t2(p, r) * z3(p, r, q)&
                )
            end do
        end do
    end do

    allocate(tau28_spsq(NAO, NAO))
    tau28_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau28_spsq(p, q) = tau28_spsq(p, q) + ( &
                    tau27_spsq(p, r) * t3(p, r, q)&
                )
            end do
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau38_spsq(p, q) = tau38_spsq(p, q) + ( &
                4 * tau28_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau28_spsq)

    allocate(tau30_spsq(NAO, NAO))
    tau30_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau30_spsq(p, q) = tau30_spsq(p, q) + ( &
                    t2(q, r) * tau27_spsq(p, r)&
                )
            end do
        end do
    end do

    allocate(tau33_spsq(NAO, NAO))
    tau33_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau33_spsq(p, q) = tau33_spsq(p, q) + ( &
                2 * tau30_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau30_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau33_spsq(p, q) = tau33_spsq(p, q) - ( &
                2 * t1(q)**2 * tau27_spsq(p, q)&
            )
    
        end do
    end do

    allocate(tau36_spsq(NAO))
    tau36_spsq = 0.0

    do p=1, NAO
        do q=1, NAO
            tau36_spsq(p) = tau36_spsq(p) + ( &
                t2(p, q) * tau27_spsq(p, q)&
            )
        end do
    end do

    allocate(tau37_spsq(NAO))
    tau37_spsq = 0.0

    do p=1, NAO
    
        tau37_spsq(p) = tau37_spsq(p) - ( &
            2 * tau36_spsq(p)&
        )
    
    end do

    deallocate(tau36_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau41_spsq(p, q) = tau41_spsq(p, q) + ( &
                2 * t1(p) * tau27_spsq(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau41_spsq(p, q) = tau41_spsq(p, q) + ( &
                2 * t1(q) * tau27_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau47_spsq(p, q) = tau47_spsq(p, q) + ( &
                2 * tau27_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau48_spsq(p, q) = tau48_spsq(p, q) + ( &
                2 * tau27_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau49_spsq(p, q) = tau49_spsq(p, q) + ( &
                6 * t1(p) * tau48_spsq(p, q)&
            )
    
        end do
    end do

    allocate(tau50_spsq(NAO, NAO))
    tau50_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau50_spsq(p, q) = tau50_spsq(p, q) + ( &
                    t2(r, p) * tau48_spsq(q, r)&
                )
            end do
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau52_spsq(p, q) = tau52_spsq(p, q) - ( &
                3 * tau50_spsq(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau58_spsq(p, q) = tau58_spsq(p, q) + ( &
                3 * tau50_spsq(q, p)&
            )
    
        end do
    end do

    deallocate(tau50_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau52_spsq(p, q) = tau52_spsq(p, q) + ( &
                3 * t1(q)**2 * tau48_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau57_spsq(p, q) = tau57_spsq(p, q) - ( &
                6 * t1(p) * tau48_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau58_spsq(p, q) = tau58_spsq(p, q) - ( &
                6 * t2(q, p) * tau48_spsq(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau64_spsq(p, q) = tau64_spsq(p, q) + ( &
                tau27_spsq(p, q)&
            )
    
        end do
    end do

    allocate(tau65_spsq(NAO))
    tau65_spsq = 0.0

    do p=1, NAO
        do q=1, NAO
            tau65_spsq(p) = tau65_spsq(p) + ( &
                t2(q, p) * tau64_spsq(p, q)&
            )
        end do
    end do

    deallocate(tau64_spsq)

    allocate(tau66_spsq(NAO))
    tau66_spsq = 0.0

    do p=1, NAO
    
        tau66_spsq(p) = tau66_spsq(p) + ( &
            3 * tau65_spsq(p)&
        )
    
    end do

    do p=1, NAO
    
        tau69_spsq(p) = tau69_spsq(p) + ( &
            6 * tau65_spsq(p)&
        )
    
    end do

    deallocate(tau65_spsq)

    allocate(tau29_spsq(NAO, NAO))
    tau29_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau29_spsq(p, q) = tau29_spsq(p, q) + ( &
                    z2(p, r) * t3(p, r, q)&
                )
            end do
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau33_spsq(p, q) = tau33_spsq(p, q) + ( &
                2 * tau29_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau52_spsq(p, q) = tau52_spsq(p, q) - ( &
                6 * tau29_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau58_spsq(p, q) = tau58_spsq(p, q) + ( &
                6 * tau29_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau29_spsq)

    allocate(tau31_spsq(NAO))
    tau31_spsq = 0.0

    do p=1, NAO
        do q=1, NAO
            tau31_spsq(p) = tau31_spsq(p) + ( &
                t2(p, q) * z2(p, q)&
            )
        end do
    end do

    do p=1, NAO
    
        tau32_spsq(p) = tau32_spsq(p) + ( &
            2 * tau31_spsq(p)&
        )
    
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau33_spsq(p, q) = tau33_spsq(p, q) + ( &
                t1(q) * tau32_spsq(p)&
            )
    
        end do
    end do

    deallocate(tau32_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau38_spsq(p, q) = tau38_spsq(p, q) + ( &
                2 * t1(p) * tau33_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau33_spsq)

    allocate(tau44_spsq(NAO))
    tau44_spsq = 0.0

    do p=1, NAO
    
        tau44_spsq(p) = tau44_spsq(p) + ( &
            tau31_spsq(p)&
        )
    
    end do

    do p=1, NAO
    
        tau51_spsq(p) = tau51_spsq(p) + ( &
            6 * tau31_spsq(p)&
        )
    
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau52_spsq(p, q) = tau52_spsq(p, q) - ( &
                t1(q) * tau51_spsq(p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau58_spsq(p, q) = tau58_spsq(p, q) + ( &
                t1(q) * tau51_spsq(p)&
            )
    
        end do
    end do

    do p=1, NAO
    
        tau66_spsq(p) = tau66_spsq(p) + ( &
            t1(p) * tau51_spsq(p)&
        )
    
    end do

    do p=1, NAO
    
        tau69_spsq(p) = tau69_spsq(p) + ( &
            2 * t1(p) * tau51_spsq(p)&
        )
    
    end do

    deallocate(tau51_spsq)

    do p=1, NAO
    
        tau70_spsq(p) = tau70_spsq(p) + ( &
            6 * tau31_spsq(p)&
        )
    
    end do

    deallocate(tau31_spsq)

    allocate(tau34_spsq(NAO))
    tau34_spsq = 0.0

    do p=1, NAO
        do q=1, NAO
            tau34_spsq(p) = tau34_spsq(p) + ( &
                z1(q) * t2(p, q)&
            )
        end do
    end do

    do p=1, NAO
    
        tau37_spsq(p) = tau37_spsq(p) + ( &
            2 * tau34_spsq(p)&
        )
    
    end do

    do p=1, NAO
    
        tau69_spsq(p) = tau69_spsq(p) - ( &
            6 * tau34_spsq(p)&
        )
    
    end do

    do p=1, NAO
    
        tau72_spsq(p) = tau72_spsq(p) + ( &
            6 * tau34_spsq(p)&
        )
    
    end do

    deallocate(tau34_spsq)

    allocate(tau35_spsq(NAO))
    tau35_spsq = 0.0

    do p=1, NAO
        do r=1, NAO
            do q=1, NAO
                tau35_spsq(p) = tau35_spsq(p) + ( &
                    z2(r, q) * t3(p, q, r)&
                )
            end do
        end do
    end do

    do p=1, NAO
    
        tau37_spsq(p) = tau37_spsq(p) + ( &
            tau35_spsq(p)&
        )
    
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau38_spsq(p, q) = tau38_spsq(p, q) - ( &
                t1(p) * tau37_spsq(q)&
            )
    
        end do
    end do

    deallocate(tau37_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau46_spsq(p, q) = tau46_spsq(p, q) - ( &
                2 * tau38_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau46_spsq(p, q) = tau46_spsq(p, q) - ( &
                2 * tau38_spsq(q, p)&
            )
    
        end do
    end do

    deallocate(tau38_spsq)

    do p=1, NAO
    
        tau69_spsq(p) = tau69_spsq(p) - ( &
            3 * tau35_spsq(p)&
        )
    
    end do

    do p=1, NAO
    
        tau72_spsq(p) = tau72_spsq(p) + ( &
            3 * tau35_spsq(p)&
        )
    
    end do

    deallocate(tau35_spsq)

    allocate(tau39_spsq(NAO, NAO, NAO, NAO))
    tau39_spsq = 0.0

    do s=1, NAO
        do r=1, NAO
            do q=1, NAO
                do p=1, NAO
    
                    tau39_spsq(p, q, r, s) = tau39_spsq(p, q, r, s) + ( &
                        t4(s, p, q, r)&
                    )
    
                end do
            end do
        end do
    end do

    do s=1, NAO
        do r=1, NAO
            do q=1, NAO
                do p=1, NAO
    
                    tau39_spsq(p, q, r, s) = tau39_spsq(p, q, r, s) + ( &
                        2 * t2(p, q) * t2(s, r)&
                    )
    
                end do
            end do
        end do
    end do

    allocate(tau40_spsq(NAO, NAO))
    tau40_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
            do s=1, NAO
                do r=1, NAO
                    tau40_spsq(p, q) = tau40_spsq(p, q) + ( &
                        tau39_spsq(r, p, q, s) * z4(r, p, q, s)&
                    )
                end do
            end do
        end do
    end do

    deallocate(tau39_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau41_spsq(p, q) = tau41_spsq(p, q) + ( &
                tau40_spsq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau42_spsq(p, q) = tau42_spsq(p, q) + ( &
                tau40_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau40_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau46_spsq(p, q) = tau46_spsq(p, q) + ( &
                8 * t1(p) * t1(q) * tau42_spsq(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau52_spsq(p, q) = tau52_spsq(p, q) + ( &
                6 * t1(q) * tau42_spsq(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau58_spsq(p, q) = tau58_spsq(p, q) - ( &
                6 * t1(q) * tau42_spsq(q, p)&
            )
    
        end do
    end do

    deallocate(tau42_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau41_spsq(p, q) = tau41_spsq(p, q) + ( &
                2 * t1(p) * t1(q) * z2(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau46_spsq(p, q) = tau46_spsq(p, q) + ( &
                8 * t2(q, p) * tau41_spsq(q, p)&
            )
    
        end do
    end do

    deallocate(tau41_spsq)

    allocate(tau43_spsq(NAO, NAO))
    tau43_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau43_spsq(p, q) = tau43_spsq(p, q) + ( &
                2 * t2(q, p)**2&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau43_spsq(p, q) = tau43_spsq(p, q) + ( &
                t1(p)**2 * t1(q)**2&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau46_spsq(p, q) = tau46_spsq(p, q) + ( &
                4 * tau43_spsq(q, p) * z2(q, p)&
            )
    
        end do
    end do

    deallocate(tau43_spsq)

    do p=1, NAO
    
        tau44_spsq(p) = tau44_spsq(p) + ( &
            t1(p) * z1(p)&
        )
    
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau46_spsq(p, q) = tau46_spsq(p, q) - ( &
                8 * tau44_spsq(p) * t2(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau46_spsq(p, q) = tau46_spsq(p, q) - ( &
                8 * tau44_spsq(q) * t2(q, p)&
            )
    
        end do
    end do

    deallocate(tau44_spsq)

    allocate(tau45_spsq(NAO))
    tau45_spsq = 0.0

    do p=1, NAO
    
        tau45_spsq(p) = tau45_spsq(p) + ( &
            t1(p)&
        )
    
    end do

    do p=1, NAO
    
        tau45_spsq(p) = tau45_spsq(p) - ( &
            t1(p)**2 * z1(p)&
        )
    
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau46_spsq(p, q) = tau46_spsq(p, q) + ( &
                4 * t1(p) * tau45_spsq(q)&
            )
    
        end do
    end do

    deallocate(tau45_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau46_spsq(p, q) = tau46_spsq(p, q) + ( &
                4 * t2(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau46_spsq(p, q) = tau46_spsq(p, q) - ( &
                4 * t1(p)**2 * t1(q) * z1(p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
            do r=1, NAO
                tau46_spsq(p, q) = tau46_spsq(p, q) + ( &
                    4 * z1(r) * t3(q, r, p)&
                )
            end do
        end do
    end do

    do q=1, NAO
        do p=1, NAO
            do s=1, NAO
                do r=1, NAO
                    tau46_spsq(p, q) = tau46_spsq(p, q) + ( &
                        2 * z2(s, r) * t4(q, r, s, p)&
                    )
                end do
            end do
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                S40(q, p) * tau46_spsq(q, p) / 4&
            )
    
        end do
    end do

    deallocate(tau46_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau47_spsq(p, q) = tau47_spsq(p, q) + ( &
                2 * t1(p) * z2(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau52_spsq(p, q) = tau52_spsq(p, q) + ( &
                6 * t2(q, p) * tau47_spsq(q, p)&
            )
    
        end do
    end do

    deallocate(tau47_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau49_spsq(p, q) = tau49_spsq(p, q) - ( &
                6 * t1(p) * z1(q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau49_spsq(p, q) = tau49_spsq(p, q) + ( &
                6 * t1(p)**2 * z2(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau52_spsq(p, q) = tau52_spsq(p, q) + ( &
                t1(p) * tau49_spsq(q, p)&
            )
    
        end do
    end do

    deallocate(tau49_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau52_spsq(p, q) = tau52_spsq(p, q) - ( &
                6 * z1(p) * t2(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                S31pq(p, q) * tau52_spsq(p, q) / 3&
            )
    
        end do
    end do

    deallocate(tau52_spsq)

    allocate(tau53_spsq(NAO, NAO))
    tau53_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau53_spsq(p, q) = tau53_spsq(p, q) + ( &
                2 * t1(p) * t2(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau53_spsq(p, q) = tau53_spsq(p, q) + ( &
                t1(p)**2 * t1(q)&
            )
    
        end do
    end do

    allocate(tau55_spsq(NAO, NAO))
    tau55_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau55_spsq(p, q) = tau55_spsq(p, q) - ( &
                2 * S31qp(q, p) * tau53_spsq(q, p)&
            )
    
        end do
    end do

    deallocate(tau53_spsq)

    allocate(tau54_spsq(NAO, NAO))
    tau54_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau54_spsq(p, q) = tau54_spsq(p, q) + ( &
                S13pq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau54_spsq(p, q) = tau54_spsq(p, q) + ( &
                2 * t1(p) * S22NN(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau55_spsq(p, q) = tau55_spsq(p, q) + ( &
                2 * t1(p) * tau54_spsq(q, p)&
            )
    
        end do
    end do

    deallocate(tau54_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau55_spsq(p, q) = tau55_spsq(p, q) + ( &
                S04(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau55_spsq(p, q) = tau55_spsq(p, q) + ( &
                2 * t1(q) * S13qp(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau55_spsq(p, q) = tau55_spsq(p, q) + ( &
                4 * S22NN(q, p) * t2(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau55_spsq(p, q) = tau55_spsq(p, q) - ( &
                t1(p)**2 * ST22pq(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau55_spsq(p, q) = tau55_spsq(p, q) - ( &
                t1(q)**2 * ST22qp(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                tau55_spsq(q, p) * z2(q, p)&
            )
    
        end do
    end do

    deallocate(tau55_spsq)

    allocate(tau56_spsq(NAO, NAO))
    tau56_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau56_spsq(p, q) = tau56_spsq(p, q) + ( &
                S13pq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau56_spsq(p, q) = tau56_spsq(p, q) + ( &
                2 * t1(p) * S22NN(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau56_spsq(p, q) = tau56_spsq(p, q) - ( &
                t1(q) * ST22pq(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau56_spsq(p, q) = tau56_spsq(p, q) - ( &
                t1(p)**2 * S31qp(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                tau48_spsq(q, p) * tau56_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau48_spsq)

    deallocate(tau56_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau57_spsq(p, q) = tau57_spsq(p, q) + ( &
                6 * t1(p) * z1(q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau58_spsq(p, q) = tau58_spsq(p, q) + ( &
                t1(p) * tau57_spsq(q, p)&
            )
    
        end do
    end do

    deallocate(tau57_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau58_spsq(p, q) = tau58_spsq(p, q) + ( &
                6 * z1(p) * t2(q, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                S31qp(p, q) * tau58_spsq(q, p) / 3&
            )
    
        end do
    end do

    deallocate(tau58_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau59_spsq(p, q) = tau59_spsq(p, q) + ( &
                6 * t1(p) * z1(q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                ST22pq(p, q) * tau59_spsq(q, p) / 6&
            )
    
        end do
    end do

    deallocate(tau59_spsq)

    allocate(tau60_spsq(NAO, NAO))
    tau60_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau60_spsq(p, q) = tau60_spsq(p, q) + ( &
                ST22qp(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau60_spsq(p, q) = tau60_spsq(p, q) - ( &
                t1(q)**2 * S40(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                tau60_spsq(p, q) * tau61_spsq(p, q) / 2&
            )
    
        end do
    end do

    deallocate(tau60_spsq)

    deallocate(tau61_spsq)

    allocate(tau62_spsq(NAO, NAO))
    tau62_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau62_spsq(p, q) = tau62_spsq(p, q) + ( &
                S13qp(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau62_spsq(p, q) = tau62_spsq(p, q) + ( &
                2 * t1(q) * S22NN(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau62_spsq(p, q) = tau62_spsq(p, q) - ( &
                t1(p) * ST22qp(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * tau27_spsq(p, q) * tau62_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau27_spsq)

    deallocate(tau62_spsq)

    allocate(tau63_spsq(NAO, NAO))
    tau63_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau63_spsq(p, q) = tau63_spsq(p, q) + ( &
                S13qp(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau63_spsq(p, q) = tau63_spsq(p, q) + ( &
                2 * t1(q) * S22NN(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                tau5_spsq(p, q) * tau63_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau5_spsq)

    deallocate(tau63_spsq)

    do p=1, NAO
    
        tau66_spsq(p) = tau66_spsq(p) + ( &
            3 * t1(p)**2 * z1(p)&
        )
    
    end do

    allocate(tau67_spsq(NAO))
    tau67_spsq = 0.0

    do p=1, NAO
    
        tau67_spsq(p) = tau67_spsq(p) + ( &
            S31pq(p, p)&
        )
    
    end do

    do p=1, NAO
    
        tau67_spsq(p) = tau67_spsq(p) + ( &
            S31qp(p, p)&
        )
    
    end do

    allocate(tau68_spsq(NAO, NAO))
    tau68_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau68_spsq(p, q) = tau68_spsq(p, q) + ( &
                2*deltaf(p, q) * tau67_spsq(p)&
            )
    
        end do
    end do

    deallocate(tau67_spsq)

    do q=1, NAO
        do p=1, NAO
    
            tau68_spsq(p, q) = tau68_spsq(p, q) + ( &
                S20p(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                tau66_spsq(p) * tau68_spsq(p, q) / 3&
            )
    
        end do
    end do

    deallocate(tau66_spsq)

    deallocate(tau68_spsq)

    do p=1, NAO
    
        tau69_spsq(p) = tau69_spsq(p) - ( &
            6 * t1(p)&
        )
    
    end do

    do p=1, NAO
    
        tau69_spsq(p) = tau69_spsq(p) + ( &
            6 * t1(p)**2 * z1(p)&
        )
    
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                tau69_spsq(q) * S20q(p, q) / 6&
            )
    
        end do
    end do

    deallocate(tau69_spsq)

    do p=1, NAO
    
        tau70_spsq(p) = tau70_spsq(p) + ( &
            6 * t1(p) * z1(p)&
        )
    
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                tau70_spsq(q) * S11q(p, q) / 3&
            )
    
        end do
    end do

    allocate(tau71_spsq(NAO, NAO))
    tau71_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau71_spsq(p, q) = tau71_spsq(p, q) + ( &
                S11p(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau71_spsq(p, q) = tau71_spsq(p, q) + ( &
                2*deltaf(p, q) * S22NN(p, p)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                tau70_spsq(p) * tau71_spsq(p, q) / 3&
            )
    
        end do
    end do

    deallocate(tau70_spsq)

    deallocate(tau71_spsq)

    do p=1, NAO
    
        tau72_spsq(p) = tau72_spsq(p) + ( &
            6 * t1(p)&
        )
    
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                tau72_spsq(p) * S20p(p, q) / 6&
            )
    
        end do
    end do

    deallocate(tau72_spsq)

    allocate(tau73_spsq(NAO, NAO))
    tau73_spsq = 0.0

    do q=1, NAO
        do p=1, NAO
    
            tau73_spsq(p, q) = tau73_spsq(p, q) + ( &
                S02q(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            tau73_spsq(p, q) = tau73_spsq(p, q) + ( &
                t1(p) * ST22qp(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                z1(q) * tau73_spsq(p, q)&
            )
    
        end do
    end do

    deallocate(tau73_spsq)

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                z1(p) * S02p(p, q)&
            )
    
        end do
    end do

    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                S00(p, q)&
            )
    
        end do
    end do

End Subroutine CCSDTQ_SpSq


double precision function deltaf(p, q)
  implicit none
  integer, intent(in) :: p, q

  if (p == q) then
    deltaf = 1.0d0
  else
    deltaf = 0.0d0
  end if

end function deltaf
End Module CCSDTQSpSq
