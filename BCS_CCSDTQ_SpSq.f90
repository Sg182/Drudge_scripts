
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


    complex, dimension(NAO) :: tau0_spsq
    complex, dimension(NAO) :: tau1_spsq
    complex, dimension(NAO, NAO) :: tau2_spsq
    complex, dimension(NAO, NAO) :: tau3_spsq
    complex, dimension(NAO, NAO, NAO, NAO) :: tau4_spsq
    complex, dimension(NAO, NAO, NAO, NAO) :: tau5_spsq
    complex, dimension(NAO, NAO, NAO) :: tau6_spsq
    complex, dimension(NAO, NAO, NAO, NAO) :: tau7_spsq
    complex, dimension(NAO, NAO, NAO, NAO) :: tau8_spsq
    complex, dimension(NAO, NAO, NAO) :: tau9_spsq
    complex, dimension(NAO, NAO, NAO, NAO) :: tau10_spsq
    complex, dimension(NAO, NAO, NAO, NAO) :: tau11_spsq
    complex, dimension(NAO, NAO, NAO) :: tau12_spsq
    complex, dimension(NAO, NAO, NAO, NAO) :: tau13_spsq
    complex, dimension(NAO, NAO, NAO, NAO) :: tau14_spsq
    complex, dimension(NAO, NAO) :: tau15_spsq
    complex, dimension(NAO, NAO) :: tau16_spsq
    complex, dimension(NAO, NAO, NAO, NAO) :: tau17_spsq
    complex, dimension(NAO, NAO, NAO) :: tau18_spsq
    complex, dimension(NAO, NAO) :: tau19_spsq
    complex, dimension(NAO, NAO) :: tau20_spsq
    complex, dimension(NAO, NAO, NAO, NAO) :: tau21_spsq
    complex, dimension(NAO, NAO, NAO, NAO) :: tau22_spsq
    complex, dimension(NAO, NAO) :: tau23_spsq
    complex, dimension(NAO, NAO, NAO, NAO, NAO) :: tau24_spsq
    complex, dimension(NAO, NAO) :: tau25_spsq
    complex, dimension(NAO, NAO, NAO, NAO, NAO) :: tau26_spsq
    complex, dimension(NAO, NAO, NAO, NAO) :: tau27_spsq
    complex, dimension(NAO, NAO, NAO, NAO) :: tau28_spsq
    complex, dimension(NAO, NAO, NAO, NAO) :: tau29_spsq
    complex, dimension(NAO, NAO, NAO, NAO) :: tau30_spsq
    complex, dimension(NAO, NAO) :: tau31_spsq
    complex, dimension(NAO, NAO, NAO) :: tau32_spsq
    complex, dimension(NAO, NAO, NAO) :: tau33_spsq
    complex, dimension(NAO, NAO) :: tau34_spsq
    complex, dimension(NAO, NAO, NAO) :: tau35_spsq
    complex, dimension(NAO, NAO) :: tau36_spsq
    complex, dimension(NAO, NAO) :: tau37_spsq
    complex, dimension(NAO, NAO, NAO) :: tau38_spsq
    complex, dimension(NAO, NAO) :: tau39_spsq
    complex, dimension(NAO, NAO) :: tau40_spsq
    complex, dimension(NAO, NAO) :: tau41_spsq
    complex, dimension(NAO, NAO, NAO) :: tau42_spsq
    complex, dimension(NAO) :: tau43_spsq
    complex, dimension(NAO, NAO) :: tau44_spsq
    complex, dimension(NAO, NAO, NAO) :: tau45_spsq
    complex, dimension(NAO, NAO, NAO) :: tau46_spsq
    complex, dimension(NAO, NAO, NAO, NAO) :: tau47_spsq
    complex, dimension(NAO, NAO, NAO, NAO) :: tau48_spsq
    complex, dimension(NAO, NAO, NAO) :: tau49_spsq
    complex, dimension(NAO, NAO, NAO) :: tau50_spsq
    complex, dimension(NAO, NAO) :: tau51_spsq
    complex, dimension(NAO, NAO) :: tau52_spsq
    complex, dimension(NAO, NAO) :: tau53_spsq
    complex, dimension(NAO, NAO) :: tau54_spsq
    complex, dimension(NAO, NAO) :: tau55_spsq
    complex, dimension(NAO) :: tau56_spsq
    complex, dimension(NAO) :: tau57_spsq
    complex, dimension(NAO) :: tau58_spsq
    complex, dimension(NAO, NAO) :: tau59_spsq
    complex, dimension(NAO, NAO) :: tau60_spsq
    complex, dimension(NAO) :: tau61_spsq
    complex, dimension(NAO) :: tau62_spsq
    complex, dimension(NAO) :: tau63_spsq
    complex, dimension(NAO) :: tau64_spsq
    complex, dimension(NAO, NAO) :: tau65_spsq
    complex, dimension(NAO) :: tau66_spsq
    complex, dimension(NAO) :: tau67_spsq
    complex, dimension(NAO, NAO) :: tau68_spsq
    complex, dimension(NAO, NAO, NAO) :: tau69_spsq
    complex, dimension(NAO, NAO, NAO) :: tau70_spsq
    complex, dimension(NAO) :: tau71_spsq
    complex, dimension(NAO) :: tau72_spsq
    complex, dimension(NAO) :: tau73_spsq
    complex, dimension(NAO) :: tau74_spsq
    complex, dimension(NAO) :: tau75_spsq
    complex, dimension(NAO, NAO) :: tau76_spsq
    complex, dimension(NAO, NAO) :: tau77_spsq
     

    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau0_spsq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau0_spsq(p) = tau0_spsq(p) + ( &
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
    do p=1, NAO
        tau1_spsq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau1_spsq(p) = tau1_spsq(p) + ( &
                        t4(s, p, q, r) * z4(s, p, q, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau2_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau2_spsq(p, q) = tau2_spsq(p, q) + ( &
                t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau2_spsq(p, q) = tau2_spsq(p, q) + ( &
                t1(p) * t1(q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau3_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau3_spsq(p, q) = tau3_spsq(p, q) - ( &
                t1(p) * tau0_spsq(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau3_spsq(p, q) = tau3_spsq(p, q) + ( &
                2 * tau1_spsq(p) * tau2_spsq(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau4_spsq(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau4_spsq(p, q, r, s) = tau4_spsq(p, q, r, s) + ( &
                        t1(p) * t3(r, p, s) * t1(q)**2 &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau4_spsq(p, q, r, s) = tau4_spsq(p, q, r, s) + ( &
                        t2(r, q) * t2(s, q) * t1(p)**2 &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau5_spsq(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau5_spsq(p, q, r, s) = tau5_spsq(p, q, r, s) + ( &
                        t4(s, p, q, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau5_spsq(p, q, r, s) = tau5_spsq(p, q, r, s) + ( &
                        2 * t2(q, p) * t2(r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau6_spsq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau6_spsq(p, q, r) = tau6_spsq(p, q, r) + ( &
                    t3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau6_spsq(p, q, r) = tau6_spsq(p, q, r) + ( &
                    2 * t1(q) * t2(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau6_spsq(p, q, r) = tau6_spsq(p, q, r) + ( &
                    2 * t1(r) * t2(p, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau7_spsq(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau7_spsq(p, q, r, s) = tau7_spsq(p, q, r, s) + ( &
                        t1(p) * t3(r, s, q) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau7_spsq(p, q, r, s) = tau7_spsq(p, q, r, s) + ( &
                        t1(r) * t3(q, s, p) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau8_spsq(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau8_spsq(p, q, r, s) = tau8_spsq(p, q, r, s) + ( &
                        tau4_spsq(p, q, r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau8_spsq(p, q, r, s) = tau8_spsq(p, q, r, s) + ( &
                        tau4_spsq(q, p, r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau8_spsq(p, q, r, s) = tau8_spsq(p, q, r, s) + ( &
                        2 * tau2_spsq(q, p) * tau5_spsq(s, p, q, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau8_spsq(p, q, r, s) = tau8_spsq(p, q, r, s) + ( &
                        2 * t3(s, p, q) * tau6_spsq(r, p, q) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau8_spsq(p, q, r, s) = tau8_spsq(p, q, r, s) + ( &
                        2 * t2(q, p) * tau7_spsq(q, s, p, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau9_spsq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau9_spsq(p, q, r) = tau9_spsq(p, q, r) + ( &
                    2 * t3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau9_spsq(p, q, r) = tau9_spsq(p, q, r) + ( &
                    2 * t1(q) * t2(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau9_spsq(p, q, r) = tau9_spsq(p, q, r) + ( &
                    t1(r) * t2(p, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau10_spsq(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau10_spsq(p, q, r, s) = tau10_spsq(p, q, r, s) + ( &
                        2 * t1(p) * t4(r, p, s, q) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau10_spsq(p, q, r, s) = tau10_spsq(p, q, r, s) + ( &
                        t3(r, s, q) * t1(p)**2 &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau10_spsq(p, q, r, s) = tau10_spsq(p, q, r, s) + ( &
                        2 * t2(q, p) * tau9_spsq(s, p, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau10_spsq(p, q, r, s) = tau10_spsq(p, q, r, s) + ( &
                        2 * tau2_spsq(r, p) * t3(s, p, q) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau11_spsq(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau11_spsq(p, q, r, s) = tau11_spsq(p, q, r, s) + ( &
                        t4(s, p, q, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau11_spsq(p, q, r, s) = tau11_spsq(p, q, r, s) + ( &
                        t1(s) * t3(q, r, p) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau11_spsq(p, q, r, s) = tau11_spsq(p, q, r, s) + ( &
                        2 * t2(q, p) * t2(r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau12_spsq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau12_spsq(p, q, r) = tau12_spsq(p, q, r) + ( &
                    t1(r) * t2(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau12_spsq(p, q, r) = tau12_spsq(p, q, r) + ( &
                    2 * t3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau13_spsq(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau13_spsq(p, q, r, s) = tau13_spsq(p, q, r, s) + ( &
                        t3(r, s, q) * t1(p)**2 &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau13_spsq(p, q, r, s) = tau13_spsq(p, q, r, s) + ( &
                        2 * t2(r, p) * t3(q, p, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau13_spsq(p, q, r, s) = tau13_spsq(p, q, r, s) + ( &
                        2 * t1(p) * tau11_spsq(s, p, q, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau13_spsq(p, q, r, s) = tau13_spsq(p, q, r, s) + ( &
                        2 * t2(q, p) * tau12_spsq(s, p, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau14_spsq(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau14_spsq(p, q, r, s) = tau14_spsq(p, q, r, s) + ( &
                        t4(s, p, q, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau14_spsq(p, q, r, s) = tau14_spsq(p, q, r, s) + ( &
                        2 * t2(q, p) * t2(r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau14_spsq(p, q, r, s) = tau14_spsq(p, q, r, s) + ( &
                        t1(q) * t3(r, s, p) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau14_spsq(p, q, r, s) = tau14_spsq(p, q, r, s) + ( &
                        t1(r) * t3(q, s, p) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau15_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau15_spsq(p, q) = tau15_spsq(p, q) + ( &
                S13pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau15_spsq(p, q) = tau15_spsq(p, q) - ( &
                t1(q) * ST22pq(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16_spsq(p, q) = tau16_spsq(p, q) + ( &
                S13qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16_spsq(p, q) = tau16_spsq(p, q) - ( &
                t1(p) * ST22qp(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau17_spsq(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau17_spsq(p, q, r, s) = tau17_spsq(p, q, r, s) - ( &
                        ST22pq(p, q) * t2(r, q) * t2(s, q) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau17_spsq(p, q, r, s) = tau17_spsq(p, q, r, s) - ( &
                        ST22qp(p, q) * t2(r, p) * t2(s, p) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau17_spsq(p, q, r, s) = tau17_spsq(p, q, r, s) + ( &
                        S40(q, p) * tau8_spsq(q, p, s, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau17_spsq(p, q, r, s) = tau17_spsq(p, q, r, s) - ( &
                        S31qp(p, q) * tau10_spsq(p, s, q, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau17_spsq(p, q, r, s) = tau17_spsq(p, q, r, s) - ( &
                        S31pq(p, q) * tau13_spsq(q, s, p, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau17_spsq(p, q, r, s) = tau17_spsq(p, q, r, s) + ( &
                        2 * S22NN(p, q) * tau14_spsq(s, p, q, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau17_spsq(p, q, r, s) = tau17_spsq(p, q, r, s) + ( &
                        tau15_spsq(p, q) * t3(s, q, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau17_spsq(p, q, r, s) = tau17_spsq(p, q, r, s) + ( &
                        tau16_spsq(p, q) * t3(s, p, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau18_spsq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau18_spsq(p, q, r) = tau18_spsq(p, q, r) + ( &
                        t2(s, p) * z4(s, p, q, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19_spsq(p, q) = tau19_spsq(p, q) + ( &
                S31pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19_spsq(p, q) = tau19_spsq(p, q) - ( &
                t1(p) * S40(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20_spsq(p, q) = tau20_spsq(p, q) + ( &
                S31qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20_spsq(p, q) = tau20_spsq(p, q) - ( &
                t1(q) * S40(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau21_spsq(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau21_spsq(p, q, r, s) = tau21_spsq(p, q, r, s) - ( &
                        S40(p, q) * z2(r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau21_spsq(p, q, r, s) = tau21_spsq(p, q, r, s) + ( &
                        2 * S40(q, p) * tau18_spsq(p, r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau21_spsq(p, q, r, s) = tau21_spsq(p, q, r, s) + ( &
                        2 * S40(p, q) * tau18_spsq(q, r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau21_spsq(p, q, r, s) = tau21_spsq(p, q, r, s) - ( &
                        2 * tau19_spsq(p, q) * z3(s, p, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau21_spsq(p, q, r, s) = tau21_spsq(p, q, r, s) - ( &
                        2 * tau20_spsq(p, q) * z3(s, q, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau22_spsq(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    do p0=1, NAO
                        tau22_spsq(p, q, r, s) = tau22_spsq(p, q, r, s) + ( &
                            t2(p0, p) * z4(p0, q, r, s) &
                        )
                    end do
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23_spsq(p, q) = tau23_spsq(p, q) + ( &
                ST22qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23_spsq(p, q) = tau23_spsq(p, q) + ( &
                2 * t1(q) * S31qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23_spsq(p, q) = tau23_spsq(p, q) - ( &
                S40(p, q) * t1(q)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    do p0=1, NAO
                        tau24_spsq(p, q, r, s, p0) = 0.0
                    end do
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    do p0=1, NAO
                        tau24_spsq(p, q, r, s, p0) = tau24_spsq(p, q, r, s, p0) + ( &
                            S40(p, q) * tau22_spsq(q, s, p0, r) &
                        )
                    end do
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    do p0=1, NAO
                        tau24_spsq(p, q, r, s, p0) = tau24_spsq(p, q, r, s, p0) + ( &
                            tau23_spsq(p, q) * z4(p0, q, r, s) &
                        )
                    end do
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau25_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau25_spsq(p, q) = tau25_spsq(p, q) + ( &
                ST22pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau25_spsq(p, q) = tau25_spsq(p, q) + ( &
                2 * t1(p) * S31pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau25_spsq(p, q) = tau25_spsq(p, q) - ( &
                S40(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    do p0=1, NAO
                        tau26_spsq(p, q, r, s, p0) = 0.0
                    end do
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    do p0=1, NAO
                        tau26_spsq(p, q, r, s, p0) = tau26_spsq(p, q, r, s, p0) + ( &
                            S40(q, p) * tau22_spsq(p, s, p0, r) &
                        )
                    end do
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    do p0=1, NAO
                        tau26_spsq(p, q, r, s, p0) = tau26_spsq(p, q, r, s, p0) + ( &
                            tau25_spsq(p, q) * z4(p0, p, r, s) &
                        )
                    end do
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau27_spsq(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau27_spsq(p, q, r, s) = tau27_spsq(p, q, r, s) - ( &
                        S31pq(p, q) * z3(r, p, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau27_spsq(p, q, r, s) = tau27_spsq(p, q, r, s) + ( &
                        S40(q, p) * tau18_spsq(p, r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau27_spsq(p, q, r, s) = tau27_spsq(p, q, r, s) + ( &
                        S40(p, q) * tau18_spsq(q, r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau28_spsq(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau28_spsq(p, q, r, s) = tau28_spsq(p, q, r, s) - ( &
                        S31qp(p, q) * z3(r, q, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau28_spsq(p, q, r, s) = tau28_spsq(p, q, r, s) + ( &
                        S40(q, p) * tau18_spsq(p, r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau28_spsq(p, q, r, s) = tau28_spsq(p, q, r, s) + ( &
                        S40(p, q) * tau18_spsq(q, r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau29_spsq(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau29_spsq(p, q, r, s) = tau29_spsq(p, q, r, s) + ( &
                        S40(p, q) * t2(r, q) * t2(s, q) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau29_spsq(p, q, r, s) = tau29_spsq(p, q, r, s) - ( &
                        tau20_spsq(p, q) * t3(s, q, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau30_spsq(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau30_spsq(p, q, r, s) = tau30_spsq(p, q, r, s) + ( &
                        S40(q, p) * t2(r, p) * t2(s, p) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau30_spsq(p, q, r, s) = tau30_spsq(p, q, r, s) - ( &
                        tau19_spsq(p, q) * t3(s, p, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau31_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau31_spsq(p, q) = tau31_spsq(p, q) + ( &
                S22NN(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau31_spsq(p, q) = tau31_spsq(p, q) + ( &
                S40(p, q) * t2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau31_spsq(p, q) = tau31_spsq(p, q) - ( &
                t1(p) * S31qp(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau32_spsq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau32_spsq(p, q, r) = tau32_spsq(p, q, r) + ( &
                    t3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau32_spsq(p, q, r) = tau32_spsq(p, q, r) + ( &
                    t1(r) * t2(p, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau33_spsq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau33_spsq(p, q, r) = tau33_spsq(p, q, r) + ( &
                    2 * t1(p) * t3(q, p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau33_spsq(p, q, r) = tau33_spsq(p, q, r) + ( &
                    t2(q, r) * t1(p)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau33_spsq(p, q, r) = tau33_spsq(p, q, r) + ( &
                    2 * t2(q, p) * t2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau34_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau34_spsq(p, q) = tau34_spsq(p, q) + ( &
                S22NN(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau34_spsq(p, q) = tau34_spsq(p, q) - ( &
                t1(q) * S31pq(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau35_spsq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau35_spsq(p, q, r) = tau35_spsq(p, q, r) + ( &
                    t2(q, r) * t1(p)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau35_spsq(p, q, r) = tau35_spsq(p, q, r) + ( &
                    2 * t2(q, p) * t2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau36_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau36_spsq(p, q) = tau36_spsq(p, q) + ( &
                ST22pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau36_spsq(p, q) = tau36_spsq(p, q) - ( &
                S40(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau37_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau37_spsq(p, q) = tau37_spsq(p, q) + ( &
                S13pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau37_spsq(p, q) = tau37_spsq(p, q) - ( &
                t1(q) * tau36_spsq(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38_spsq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38_spsq(p, q, r) = tau38_spsq(p, q, r) + ( &
                    2 * tau31_spsq(p, q) * tau32_spsq(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38_spsq(p, q, r) = tau38_spsq(p, q, r) - ( &
                    tau19_spsq(p, q) * tau33_spsq(q, r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38_spsq(p, q, r) = tau38_spsq(p, q, r) + ( &
                    2 * t1(p) * t2(q, r) * tau34_spsq(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38_spsq(p, q, r) = tau38_spsq(p, q, r) - ( &
                    S31qp(p, q) * tau35_spsq(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38_spsq(p, q, r) = tau38_spsq(p, q, r) + ( &
                    t2(r, q) * tau37_spsq(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38_spsq(p, q, r) = tau38_spsq(p, q, r) + ( &
                    t2(r, p) * tau16_spsq(p, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau39_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau39_spsq(p, q) = tau39_spsq(p, q) + ( &
                    t2(r, p) * z3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau40_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau40_spsq(p, q) = tau40_spsq(p, q) + ( &
                        t3(s, p, r) * z4(s, p, q, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau41_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau41_spsq(p, q) = tau41_spsq(p, q) + ( &
                2 * tau39_spsq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau41_spsq(p, q) = tau41_spsq(p, q) + ( &
                tau40_spsq(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau42_spsq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau42_spsq(p, q, r) = tau42_spsq(p, q, r) - ( &
                    z1(r) * S40(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau42_spsq(p, q, r) = tau42_spsq(p, q, r) + ( &
                    S40(q, p) * tau41_spsq(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau42_spsq(p, q, r) = tau42_spsq(p, q, r) + ( &
                    S40(p, q) * tau41_spsq(q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau42_spsq(p, q, r) = tau42_spsq(p, q, r) - ( &
                    2 * tau19_spsq(p, q) * z2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau42_spsq(p, q, r) = tau42_spsq(p, q, r) - ( &
                    2 * tau20_spsq(p, q) * z2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau43_spsq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau43_spsq(p) = tau43_spsq(p) + ( &
            S31pq(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau43_spsq(p) = tau43_spsq(p) + ( &
            S31qp(p, p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau44_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau44_spsq(p, q) = tau44_spsq(p, q) + ( &
                S20p(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau44_spsq(p, q) = tau44_spsq(p, q) + ( &
                2*deltaf(p, q) * tau43_spsq(p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau45_spsq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau45_spsq(p, q, r) = tau45_spsq(p, q, r) + ( &
                        t2(s, p) * z3(s, q, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau46_spsq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau46_spsq(p, q, r) = tau46_spsq(p, q, r) - ( &
                    2 * z3(q, p, r) * t1(p)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau46_spsq(p, q, r) = tau46_spsq(p, q, r) + ( &
                    2 * tau45_spsq(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    do p0=1, NAO
                        tau46_spsq(p, q, r) = tau46_spsq(p, q, r) + ( &
                            t3(p0, p, s) * z4(p0, r, q, s) &
                        )
                    end do
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau47_spsq(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau47_spsq(p, q, r, s) = tau47_spsq(p, q, r, s) - ( &
                        2 * ST22qp(p, q) * z3(r, q, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau47_spsq(p, q, r, s) = tau47_spsq(p, q, r, s) - ( &
                        4 * S31qp(p, q) * tau18_spsq(q, r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau47_spsq(p, q, r, s) = tau47_spsq(p, q, r, s) + ( &
                        4 * tau44_spsq(p, q) * tau18_spsq(p, s, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau47_spsq(p, q, r, s) = tau47_spsq(p, q, r, s) - ( &
                        S40(q, p) * tau46_spsq(q, s, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau48_spsq(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau48_spsq(p, q, r, s) = tau48_spsq(p, q, r, s) + ( &
                        S40(q, p) * tau45_spsq(p, r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau48_spsq(p, q, r, s) = tau48_spsq(p, q, r, s) + ( &
                        2 * S31pq(p, q) * tau18_spsq(p, r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau48_spsq(p, q, r, s) = tau48_spsq(p, q, r, s) - ( &
                        2 * S20q(p, q) * tau18_spsq(q, r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau48_spsq(p, q, r, s) = tau48_spsq(p, q, r, s) + ( &
                        tau36_spsq(p, q) * z3(s, p, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau49_spsq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau49_spsq(p, q, r) = tau49_spsq(p, q, r) - ( &
                    2 * S31pq(p, q) * z2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau49_spsq(p, q, r) = tau49_spsq(p, q, r) + ( &
                    2 * S40(q, p) * tau39_spsq(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau49_spsq(p, q, r) = tau49_spsq(p, q, r) + ( &
                    S40(p, q) * tau39_spsq(q, r) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau50_spsq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau50_spsq(p, q, r) = tau50_spsq(p, q, r) + ( &
                    2 * S31qp(p, q) * z2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau50_spsq(p, q, r) = tau50_spsq(p, q, r) - ( &
                    S40(q, p) * tau39_spsq(p, r) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau51_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau51_spsq(p, q) = tau51_spsq(p, q) + ( &
                S22NN(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau51_spsq(p, q) = tau51_spsq(p, q) - ( &
                t1(p) * S31qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau51_spsq(p, q) = tau51_spsq(p, q) - ( &
                t1(q) * tau19_spsq(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau52_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau52_spsq(p, q) = tau52_spsq(p, q) + ( &
                2 * t2(q, p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau52_spsq(p, q) = tau52_spsq(p, q) + ( &
                t1(p)**2 * t1(q)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau53_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau53_spsq(p, q) = tau53_spsq(p, q) + ( &
                S13pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau53_spsq(p, q) = tau53_spsq(p, q) + ( &
                2 * t1(p) * S22NN(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau53_spsq(p, q) = tau53_spsq(p, q) - ( &
                S31qp(p, q) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau54_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau54_spsq(p, q) = tau54_spsq(p, q) + ( &
                ST22pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau54_spsq(p, q) = tau54_spsq(p, q) + ( &
                2 * t1(p) * S31pq(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55_spsq(p, q) = tau55_spsq(p, q) + ( &
                S04(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55_spsq(p, q) = tau55_spsq(p, q) + ( &
                2 * t1(q) * S13qp(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55_spsq(p, q) = tau55_spsq(p, q) - ( &
                ST22qp(q, p) * t1(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55_spsq(p, q) = tau55_spsq(p, q) + ( &
                4 * t2(q, p) * tau51_spsq(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55_spsq(p, q) = tau55_spsq(p, q) + ( &
                S40(q, p) * tau52_spsq(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55_spsq(p, q) = tau55_spsq(p, q) + ( &
                2 * t1(p) * tau53_spsq(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55_spsq(p, q) = tau55_spsq(p, q) - ( &
                tau54_spsq(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau56_spsq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau56_spsq(p) = tau56_spsq(p) + ( &
                t2(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau57_spsq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau57_spsq(p) = tau57_spsq(p) + ( &
                    t3(r, p, q) * z3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau58_spsq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau58_spsq(p) = tau58_spsq(p) + ( &
            6 * tau56_spsq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau58_spsq(p) = tau58_spsq(p) + ( &
            3 * tau57_spsq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau58_spsq(p) = tau58_spsq(p) + ( &
            tau1_spsq(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau59_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau59_spsq(p, q) = tau59_spsq(p, q) + ( &
                    6 * t2(r, q) * tau39_spsq(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau59_spsq(p, q) = tau59_spsq(p, q) + ( &
                6 * z1(p) * tau2_spsq(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau59_spsq(p, q) = tau59_spsq(p, q) + ( &
                t1(q) * tau58_spsq(p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau60_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau60_spsq(p, q) = tau60_spsq(p, q) + ( &
                    t2(r, p) * z2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau61_spsq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau61_spsq(p) = tau61_spsq(p) + ( &
            2 * tau56_spsq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau61_spsq(p) = tau61_spsq(p) + ( &
            tau57_spsq(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau62_spsq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau62_spsq(p) = tau62_spsq(p) + ( &
                z1(q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau63_spsq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau63_spsq(p) = tau63_spsq(p) + ( &
                    z2(r, q) * t3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau64_spsq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau64_spsq(p) = tau64_spsq(p) + ( &
            2 * tau62_spsq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau64_spsq(p) = tau64_spsq(p) + ( &
            tau63_spsq(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65_spsq(p, q) = tau65_spsq(p, q) + ( &
                2 * tau60_spsq(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65_spsq(p, q) = tau65_spsq(p, q) + ( &
                2 * tau57_spsq(p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65_spsq(p, q) = tau65_spsq(p, q) + ( &
                2 * t1(p) * t1(q) * tau61_spsq(p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65_spsq(p, q) = tau65_spsq(p, q) - ( &
                t1(p) * tau64_spsq(q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau66_spsq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau66_spsq(p) = tau66_spsq(p) + ( &
            t1(p) * z1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau66_spsq(p) = tau66_spsq(p) + ( &
            tau56_spsq(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau67_spsq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau67_spsq(p) = tau67_spsq(p) + ( &
            t1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau67_spsq(p) = tau67_spsq(p) - ( &
            z1(p) * t1(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68_spsq(p, q) = tau68_spsq(p, q) - ( &
                2 * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68_spsq(p, q) = tau68_spsq(p, q) + ( &
                2 * t1(q) * z1(p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau68_spsq(p, q) = tau68_spsq(p, q) - ( &
                    2 * t2(r, q) * tau60_spsq(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68_spsq(p, q) = tau68_spsq(p, q) + ( &
                tau65_spsq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68_spsq(p, q) = tau68_spsq(p, q) + ( &
                tau65_spsq(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68_spsq(p, q) = tau68_spsq(p, q) + ( &
                4 * tau66_spsq(p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68_spsq(p, q) = tau68_spsq(p, q) + ( &
                4 * tau66_spsq(q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68_spsq(p, q) = tau68_spsq(p, q) - ( &
                2 * t1(p) * tau67_spsq(q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau69_spsq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau69_spsq(p, q, r) = tau69_spsq(p, q, r) - ( &
                    ST22qp(p, q) * z2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau69_spsq(p, q, r) = tau69_spsq(p, q, r) + ( &
                    tau39_spsq(p, r) * tau44_spsq(p, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau70_spsq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau70_spsq(p, q, r) = tau70_spsq(p, q, r) + ( &
                    ST22pq(p, q) * z2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau70_spsq(p, q, r) = tau70_spsq(p, q, r) - ( &
                    S20q(p, q) * tau39_spsq(q, r) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau71_spsq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau71_spsq(p) = tau71_spsq(p) - ( &
            S22NN(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau71_spsq(p) = tau71_spsq(p) + ( &
            t1(p) * tau43_spsq(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau72_spsq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau72_spsq(p) = tau72_spsq(p) - ( &
            2 * t1(p) * S22NN(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau72_spsq(p) = tau72_spsq(p) + ( &
            tau43_spsq(p) * t1(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau73_spsq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau73_spsq(p) = tau73_spsq(p) + ( &
                        tau71_spsq(p) * t4(s, p, q, r) * z4(q, p, r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau73_spsq(p) = tau73_spsq(p) + ( &
                    3 * tau71_spsq(p) * t3(r, p, q) * z3(q, p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau73_spsq(p) = tau73_spsq(p) + ( &
                6 * tau71_spsq(p) * t2(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau73_spsq(p) = tau73_spsq(p) + ( &
            3 * tau72_spsq(p) * z1(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau74_spsq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau74_spsq(p) = tau74_spsq(p) - ( &
            6 * t1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau74_spsq(p) = tau74_spsq(p) + ( &
            6 * z1(p) * t1(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau74_spsq(p) = tau74_spsq(p) - ( &
            6 * tau62_spsq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau74_spsq(p) = tau74_spsq(p) - ( &
            3 * tau63_spsq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau74_spsq(p) = tau74_spsq(p) - ( &
            tau0_spsq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau74_spsq(p) = tau74_spsq(p) + ( &
            2 * t1(p) * tau58_spsq(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau75_spsq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau75_spsq(p) = tau75_spsq(p) + ( &
            6 * t1(p) * z1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau75_spsq(p) = tau75_spsq(p) + ( &
            6 * tau56_spsq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau75_spsq(p) = tau75_spsq(p) + ( &
            3 * tau57_spsq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau75_spsq(p) = tau75_spsq(p) + ( &
            tau1_spsq(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau76_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau76_spsq(p, q) = tau76_spsq(p, q) + ( &
                S02p(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau76_spsq(p, q) = tau76_spsq(p, q) + ( &
                t1(q) * ST22pq(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau77_spsq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau77_spsq(p, q) = tau77_spsq(p, q) + ( &
                S02q(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau77_spsq(p, q) = tau77_spsq(p, q) + ( &
                t1(p) * ST22qp(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                S00(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                S40(q, p) * tau3_spsq(p, q) / 6 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                S40(p, q) * tau3_spsq(q, p) / 6 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    SpSq(p, q) = SpSq(p, q) + ( &
                        tau17_spsq(p, q, r, s) * z4(r, p, q, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    SpSq(p, q) = SpSq(p, q) - ( &
                        t4(r, p, q, s) * tau21_spsq(p, q, r, s) / 2 &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    do p0=1, NAO
                        SpSq(p, q) = SpSq(p, q) + ( &
                            t4(r, p, s, p0) * tau24_spsq(p, q, r, s, p0) / 6 &
                        )
                    end do
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    do p0=1, NAO
                        SpSq(p, q) = SpSq(p, q) + ( &
                            t4(r, q, s, p0) * tau26_spsq(p, q, r, s, p0) / 6 &
                        )
                    end do
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    SpSq(p, q) = SpSq(p, q) - ( &
                        t1(p) * t3(s, r, q) * tau27_spsq(p, q, r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    SpSq(p, q) = SpSq(p, q) - ( &
                        t1(q) * t3(s, r, p) * tau28_spsq(p, q, r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    SpSq(p, q) = SpSq(p, q) - ( &
                        tau22_spsq(p, r, q, s) * tau29_spsq(p, q, r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    SpSq(p, q) = SpSq(p, q) - ( &
                        tau22_spsq(q, r, p, s) * tau30_spsq(p, q, r, s) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                SpSq(p, q) = SpSq(p, q) + ( &
                    2 * tau38_spsq(p, q, r) * z3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                SpSq(p, q) = SpSq(p, q) - ( &
                    t3(r, p, q) * tau42_spsq(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    SpSq(p, q) = SpSq(p, q) - ( &
                        t3(r, p, s) * tau47_spsq(p, q, r, s) / 4 &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    SpSq(p, q) = SpSq(p, q) + ( &
                        t3(r, q, s) * tau48_spsq(p, q, r, s) / 2 &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                SpSq(p, q) = SpSq(p, q) - ( &
                    t1(p) * t2(q, r) * tau49_spsq(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                SpSq(p, q) = SpSq(p, q) + ( &
                    2 * t2(r, q) * tau20_spsq(p, q) * tau45_spsq(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                SpSq(p, q) = SpSq(p, q) + ( &
                    t1(q) * t2(p, r) * tau50_spsq(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                tau55_spsq(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                S31pq(p, q) * tau59_spsq(p, q) / 3 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                S40(q, p) * tau68_spsq(q, p) / 2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                SpSq(p, q) = SpSq(p, q) - ( &
                    t2(r, p) * tau69_spsq(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * z1(q) * S31qp(p, q) * tau2_spsq(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                t1(p) * tau58_spsq(q) * S31qp(p, q) / 3 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                SpSq(p, q) = SpSq(p, q) + ( &
                    t2(r, q) * tau70_spsq(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                2*deltaf(p, q) * tau73_spsq(p) / 3 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                tau74_spsq(p) * S20p(p, q) / 6 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                tau74_spsq(q) * S20q(p, q) / 6 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                tau75_spsq(p) * S11p(p, q) / 3 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                tau75_spsq(q) * S11q(p, q) / 3 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                z1(p) * tau76_spsq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                z1(q) * tau77_spsq(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel

    !do p = 1, NAO
	!SpSq(p,p) = 0.75_pr
    !do q = p+1, NAO
    !   tmp = 0.5_pr * (SpSq(p,q) + (SpSq(q,p)))
    !   SpSq(p,q) = tmp
    !   SpSq(q,p) = (tmp)
    !end do
    !end do
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

