
 Module CCSDTQNpNq
    Use Precision
    Use Constants

    Contains

subroutine CCSDTQ_NpNq(NpNq, T1, T2,T3,T4, z1, z2,z3,z4, NAO, &
     N00, N20p, N20q, N11p, N11q, N02p, N02q, &
     N40, N31pq, N31qp, N22NN, NT22pq, NT22qp, N13pq, N13qp, N04)

    use Precision
    implicit none
    integer, intent(in) :: NAO
    complex(kind=pr), intent(in) :: T1(NAO), T2(NAO,NAO)
    complex(kind=pr), intent(in) :: z1(NAO), z2(NAO,NAO)

    complex(kind=pr), intent(in) :: N00(NAO,NAO)
    complex(kind=pr), intent(in) :: N20p(NAO,NAO), N20q(NAO,NAO)
    complex(kind=pr), intent(in) :: N11p(NAO,NAO), N11q(NAO,NAO)
    complex(kind=pr), intent(in) :: N02p(NAO,NAO), N02q(NAO,NAO)
    complex(kind=pr), intent(in) :: N40(NAO,NAO), N04(NAO,NAO)
    complex(kind=pr), intent(in) :: N31pq(NAO,NAO), N31qp(NAO,NAO)
    complex(kind=pr), intent(in) :: N22NN(NAO,NAO)
    complex(kind=pr), intent(in) :: NT22pq(NAO,NAO), NT22qp(NAO,NAO)
    complex(kind=pr), intent(in) :: N13pq(NAO,NAO), N13qp(NAO,NAO)
    integer                      :: p,q,r,s,i,j,k,l
    complex(kind=pr), intent(out) :: NpNq(NAO,NAO)
    complex(kind=pr) :: tmp


    complex(kind=pr), dimension(NAO) :: tau0_npnq
    complex(kind=pr), dimension(NAO) :: tau1_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau2_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau3_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau4_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau5_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau6_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau7_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau8_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau9_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau10_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau11_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau12_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau13_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau14_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau15_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau16_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau17_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau18_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau19_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau20_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau21_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau22_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau23_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO, NAO) :: tau24_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau25_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO, NAO) :: tau26_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau27_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau28_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau29_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau30_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau31_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau32_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau33_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau34_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau35_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau36_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau37_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau38_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau39_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau40_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau41_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau42_npnq
    complex(kind=pr), dimension(NAO) :: tau43_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau44_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau45_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau46_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau47_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau48_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau49_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau50_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau51_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau52_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau53_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau54_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau55_npnq
    complex(kind=pr), dimension(NAO) :: tau56_npnq
    complex(kind=pr), dimension(NAO) :: tau57_npnq
    complex(kind=pr), dimension(NAO) :: tau58_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau59_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau60_npnq
    complex(kind=pr), dimension(NAO) :: tau61_npnq
    complex(kind=pr), dimension(NAO) :: tau62_npnq
    complex(kind=pr), dimension(NAO) :: tau63_npnq
    complex(kind=pr), dimension(NAO) :: tau64_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau65_npnq
    complex(kind=pr), dimension(NAO) :: tau66_npnq
    complex(kind=pr), dimension(NAO) :: tau67_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau68_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau69_npnq
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau70_npnq
    complex(kind=pr), dimension(NAO) :: tau71_npnq
    complex(kind=pr), dimension(NAO) :: tau72_npnq
    complex(kind=pr), dimension(NAO) :: tau73_npnq
    complex(kind=pr), dimension(NAO) :: tau74_npnq
    complex(kind=pr), dimension(NAO) :: tau75_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau76_npnq
    complex(kind=pr), dimension(NAO, NAO) :: tau77_npnq

    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau0_npnq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau0_npnq(p) = tau0_npnq(p) + ( &
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
        tau1_npnq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau1_npnq(p) = tau1_npnq(p) + ( &
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
            tau2_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau2_npnq(p, q) = tau2_npnq(p, q) + ( &
                t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau2_npnq(p, q) = tau2_npnq(p, q) + ( &
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
            tau3_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau3_npnq(p, q) = tau3_npnq(p, q) - ( &
                t1(p) * tau0_npnq(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau3_npnq(p, q) = tau3_npnq(p, q) + ( &
                2 * tau1_npnq(p) * tau2_npnq(q, p) &
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
                    tau4_npnq(p, q, r, s) = 0.0
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
                    tau4_npnq(p, q, r, s) = tau4_npnq(p, q, r, s) + ( &
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
                    tau4_npnq(p, q, r, s) = tau4_npnq(p, q, r, s) + ( &
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
                    tau5_npnq(p, q, r, s) = 0.0
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
                    tau5_npnq(p, q, r, s) = tau5_npnq(p, q, r, s) + ( &
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
                    tau5_npnq(p, q, r, s) = tau5_npnq(p, q, r, s) + ( &
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
                tau6_npnq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau6_npnq(p, q, r) = tau6_npnq(p, q, r) + ( &
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
                tau6_npnq(p, q, r) = tau6_npnq(p, q, r) + ( &
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
                tau6_npnq(p, q, r) = tau6_npnq(p, q, r) + ( &
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
                    tau7_npnq(p, q, r, s) = 0.0
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
                    tau7_npnq(p, q, r, s) = tau7_npnq(p, q, r, s) + ( &
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
                    tau7_npnq(p, q, r, s) = tau7_npnq(p, q, r, s) + ( &
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
                    tau8_npnq(p, q, r, s) = 0.0
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
                    tau8_npnq(p, q, r, s) = tau8_npnq(p, q, r, s) + ( &
                        tau4_npnq(p, q, r, s) &
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
                    tau8_npnq(p, q, r, s) = tau8_npnq(p, q, r, s) + ( &
                        tau4_npnq(q, p, r, s) &
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
                    tau8_npnq(p, q, r, s) = tau8_npnq(p, q, r, s) + ( &
                        2 * tau2_npnq(q, p) * tau5_npnq(s, p, q, r) &
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
                    tau8_npnq(p, q, r, s) = tau8_npnq(p, q, r, s) + ( &
                        2 * t3(s, p, q) * tau6_npnq(r, p, q) &
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
                    tau8_npnq(p, q, r, s) = tau8_npnq(p, q, r, s) + ( &
                        2 * t2(q, p) * tau7_npnq(q, s, p, r) &
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
                tau9_npnq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau9_npnq(p, q, r) = tau9_npnq(p, q, r) + ( &
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
                tau9_npnq(p, q, r) = tau9_npnq(p, q, r) + ( &
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
                tau9_npnq(p, q, r) = tau9_npnq(p, q, r) + ( &
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
                    tau10_npnq(p, q, r, s) = 0.0
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
                    tau10_npnq(p, q, r, s) = tau10_npnq(p, q, r, s) + ( &
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
                    tau10_npnq(p, q, r, s) = tau10_npnq(p, q, r, s) + ( &
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
                    tau10_npnq(p, q, r, s) = tau10_npnq(p, q, r, s) + ( &
                        2 * t2(q, p) * tau9_npnq(s, p, r) &
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
                    tau10_npnq(p, q, r, s) = tau10_npnq(p, q, r, s) + ( &
                        2 * tau2_npnq(r, p) * t3(s, p, q) &
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
                    tau11_npnq(p, q, r, s) = 0.0
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
                    tau11_npnq(p, q, r, s) = tau11_npnq(p, q, r, s) + ( &
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
                    tau11_npnq(p, q, r, s) = tau11_npnq(p, q, r, s) + ( &
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
                    tau11_npnq(p, q, r, s) = tau11_npnq(p, q, r, s) + ( &
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
                tau12_npnq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau12_npnq(p, q, r) = tau12_npnq(p, q, r) + ( &
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
                tau12_npnq(p, q, r) = tau12_npnq(p, q, r) + ( &
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
                    tau13_npnq(p, q, r, s) = 0.0
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
                    tau13_npnq(p, q, r, s) = tau13_npnq(p, q, r, s) + ( &
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
                    tau13_npnq(p, q, r, s) = tau13_npnq(p, q, r, s) + ( &
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
                    tau13_npnq(p, q, r, s) = tau13_npnq(p, q, r, s) + ( &
                        2 * t1(p) * tau11_npnq(s, p, q, r) &
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
                    tau13_npnq(p, q, r, s) = tau13_npnq(p, q, r, s) + ( &
                        2 * t2(q, p) * tau12_npnq(s, p, r) &
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
                    tau14_npnq(p, q, r, s) = 0.0
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
                    tau14_npnq(p, q, r, s) = tau14_npnq(p, q, r, s) + ( &
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
                    tau14_npnq(p, q, r, s) = tau14_npnq(p, q, r, s) + ( &
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
                    tau14_npnq(p, q, r, s) = tau14_npnq(p, q, r, s) + ( &
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
                    tau14_npnq(p, q, r, s) = tau14_npnq(p, q, r, s) + ( &
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
            tau15_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau15_npnq(p, q) = tau15_npnq(p, q) + ( &
                N13pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau15_npnq(p, q) = tau15_npnq(p, q) - ( &
                t1(q) * NT22pq(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16_npnq(p, q) = tau16_npnq(p, q) + ( &
                N13qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16_npnq(p, q) = tau16_npnq(p, q) - ( &
                t1(p) * NT22qp(p, q) &
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
                    tau17_npnq(p, q, r, s) = 0.0
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
                    tau17_npnq(p, q, r, s) = tau17_npnq(p, q, r, s) - ( &
                        NT22pq(p, q) * t2(r, q) * t2(s, q) &
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
                    tau17_npnq(p, q, r, s) = tau17_npnq(p, q, r, s) - ( &
                        NT22qp(p, q) * t2(r, p) * t2(s, p) &
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
                    tau17_npnq(p, q, r, s) = tau17_npnq(p, q, r, s) + ( &
                        N40(q, p) * tau8_npnq(q, p, s, r) &
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
                    tau17_npnq(p, q, r, s) = tau17_npnq(p, q, r, s) - ( &
                        N31qp(p, q) * tau10_npnq(p, s, q, r) &
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
                    tau17_npnq(p, q, r, s) = tau17_npnq(p, q, r, s) - ( &
                        N31pq(p, q) * tau13_npnq(q, s, p, r) &
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
                    tau17_npnq(p, q, r, s) = tau17_npnq(p, q, r, s) + ( &
                        2 * N22NN(p, q) * tau14_npnq(s, p, q, r) &
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
                    tau17_npnq(p, q, r, s) = tau17_npnq(p, q, r, s) + ( &
                        tau15_npnq(p, q) * t3(s, q, r) &
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
                    tau17_npnq(p, q, r, s) = tau17_npnq(p, q, r, s) + ( &
                        tau16_npnq(p, q) * t3(s, p, r) &
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
                tau18_npnq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau18_npnq(p, q, r) = tau18_npnq(p, q, r) + ( &
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
            tau19_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19_npnq(p, q) = tau19_npnq(p, q) + ( &
                N31pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19_npnq(p, q) = tau19_npnq(p, q) - ( &
                t1(p) * N40(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20_npnq(p, q) = tau20_npnq(p, q) + ( &
                N31qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20_npnq(p, q) = tau20_npnq(p, q) - ( &
                t1(q) * N40(p, q) &
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
                    tau21_npnq(p, q, r, s) = 0.0
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
                    tau21_npnq(p, q, r, s) = tau21_npnq(p, q, r, s) - ( &
                        N40(p, q) * z2(r, s) &
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
                    tau21_npnq(p, q, r, s) = tau21_npnq(p, q, r, s) + ( &
                        2 * N40(q, p) * tau18_npnq(p, r, s) &
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
                    tau21_npnq(p, q, r, s) = tau21_npnq(p, q, r, s) + ( &
                        2 * N40(p, q) * tau18_npnq(q, r, s) &
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
                    tau21_npnq(p, q, r, s) = tau21_npnq(p, q, r, s) - ( &
                        2 * tau19_npnq(p, q) * z3(s, p, r) &
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
                    tau21_npnq(p, q, r, s) = tau21_npnq(p, q, r, s) - ( &
                        2 * tau20_npnq(p, q) * z3(s, q, r) &
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
                    tau22_npnq(p, q, r, s) = 0.0
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
                        tau22_npnq(p, q, r, s) = tau22_npnq(p, q, r, s) + ( &
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
            tau23_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23_npnq(p, q) = tau23_npnq(p, q) + ( &
                NT22qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23_npnq(p, q) = tau23_npnq(p, q) + ( &
                2 * t1(q) * N31qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23_npnq(p, q) = tau23_npnq(p, q) - ( &
                N40(p, q) * t1(q)**2 &
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
                        tau24_npnq(p, q, r, s, p0) = 0.0
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
                        tau24_npnq(p, q, r, s, p0) = tau24_npnq(p, q, r, s, p0) + ( &
                            N40(p, q) * tau22_npnq(q, s, p0, r) &
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
                        tau24_npnq(p, q, r, s, p0) = tau24_npnq(p, q, r, s, p0) + ( &
                            tau23_npnq(p, q) * z4(p0, q, r, s) &
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
            tau25_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau25_npnq(p, q) = tau25_npnq(p, q) + ( &
                NT22pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau25_npnq(p, q) = tau25_npnq(p, q) + ( &
                2 * t1(p) * N31pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau25_npnq(p, q) = tau25_npnq(p, q) - ( &
                N40(q, p) * t1(p)**2 &
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
                        tau26_npnq(p, q, r, s, p0) = 0.0
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
                        tau26_npnq(p, q, r, s, p0) = tau26_npnq(p, q, r, s, p0) + ( &
                            N40(q, p) * tau22_npnq(p, s, p0, r) &
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
                        tau26_npnq(p, q, r, s, p0) = tau26_npnq(p, q, r, s, p0) + ( &
                            tau25_npnq(p, q) * z4(p0, p, r, s) &
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
                    tau27_npnq(p, q, r, s) = 0.0
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
                    tau27_npnq(p, q, r, s) = tau27_npnq(p, q, r, s) - ( &
                        N31pq(p, q) * z3(r, p, s) &
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
                    tau27_npnq(p, q, r, s) = tau27_npnq(p, q, r, s) + ( &
                        N40(q, p) * tau18_npnq(p, r, s) &
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
                    tau27_npnq(p, q, r, s) = tau27_npnq(p, q, r, s) + ( &
                        N40(p, q) * tau18_npnq(q, r, s) &
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
                    tau28_npnq(p, q, r, s) = 0.0
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
                    tau28_npnq(p, q, r, s) = tau28_npnq(p, q, r, s) - ( &
                        N31qp(p, q) * z3(r, q, s) &
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
                    tau28_npnq(p, q, r, s) = tau28_npnq(p, q, r, s) + ( &
                        N40(q, p) * tau18_npnq(p, r, s) &
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
                    tau28_npnq(p, q, r, s) = tau28_npnq(p, q, r, s) + ( &
                        N40(p, q) * tau18_npnq(q, r, s) &
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
                    tau29_npnq(p, q, r, s) = 0.0
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
                    tau29_npnq(p, q, r, s) = tau29_npnq(p, q, r, s) + ( &
                        N40(p, q) * t2(r, q) * t2(s, q) &
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
                    tau29_npnq(p, q, r, s) = tau29_npnq(p, q, r, s) - ( &
                        tau20_npnq(p, q) * t3(s, q, r) &
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
                    tau30_npnq(p, q, r, s) = 0.0
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
                    tau30_npnq(p, q, r, s) = tau30_npnq(p, q, r, s) + ( &
                        N40(q, p) * t2(r, p) * t2(s, p) &
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
                    tau30_npnq(p, q, r, s) = tau30_npnq(p, q, r, s) - ( &
                        tau19_npnq(p, q) * t3(s, p, r) &
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
            tau31_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau31_npnq(p, q) = tau31_npnq(p, q) + ( &
                N22NN(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau31_npnq(p, q) = tau31_npnq(p, q) + ( &
                N40(p, q) * t2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau31_npnq(p, q) = tau31_npnq(p, q) - ( &
                t1(p) * N31qp(p, q) &
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
                tau32_npnq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau32_npnq(p, q, r) = tau32_npnq(p, q, r) + ( &
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
                tau32_npnq(p, q, r) = tau32_npnq(p, q, r) + ( &
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
                tau33_npnq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau33_npnq(p, q, r) = tau33_npnq(p, q, r) + ( &
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
                tau33_npnq(p, q, r) = tau33_npnq(p, q, r) + ( &
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
                tau33_npnq(p, q, r) = tau33_npnq(p, q, r) + ( &
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
            tau34_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau34_npnq(p, q) = tau34_npnq(p, q) + ( &
                N22NN(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau34_npnq(p, q) = tau34_npnq(p, q) - ( &
                t1(q) * N31pq(p, q) &
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
                tau35_npnq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau35_npnq(p, q, r) = tau35_npnq(p, q, r) + ( &
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
                tau35_npnq(p, q, r) = tau35_npnq(p, q, r) + ( &
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
            tau36_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau36_npnq(p, q) = tau36_npnq(p, q) + ( &
                NT22pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau36_npnq(p, q) = tau36_npnq(p, q) - ( &
                N40(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau37_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau37_npnq(p, q) = tau37_npnq(p, q) + ( &
                N13pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau37_npnq(p, q) = tau37_npnq(p, q) - ( &
                t1(q) * tau36_npnq(p, q) &
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
                tau38_npnq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38_npnq(p, q, r) = tau38_npnq(p, q, r) + ( &
                    2 * tau31_npnq(p, q) * tau32_npnq(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38_npnq(p, q, r) = tau38_npnq(p, q, r) - ( &
                    tau19_npnq(p, q) * tau33_npnq(q, r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38_npnq(p, q, r) = tau38_npnq(p, q, r) + ( &
                    2 * t1(p) * t2(q, r) * tau34_npnq(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38_npnq(p, q, r) = tau38_npnq(p, q, r) - ( &
                    N31qp(p, q) * tau35_npnq(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38_npnq(p, q, r) = tau38_npnq(p, q, r) + ( &
                    t2(r, q) * tau37_npnq(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38_npnq(p, q, r) = tau38_npnq(p, q, r) + ( &
                    t2(r, p) * tau16_npnq(p, q) &
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
            tau39_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau39_npnq(p, q) = tau39_npnq(p, q) + ( &
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
            tau40_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau40_npnq(p, q) = tau40_npnq(p, q) + ( &
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
            tau41_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau41_npnq(p, q) = tau41_npnq(p, q) + ( &
                2 * tau39_npnq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau41_npnq(p, q) = tau41_npnq(p, q) + ( &
                tau40_npnq(p, q) &
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
                tau42_npnq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau42_npnq(p, q, r) = tau42_npnq(p, q, r) - ( &
                    z1(r) * N40(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau42_npnq(p, q, r) = tau42_npnq(p, q, r) + ( &
                    N40(q, p) * tau41_npnq(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau42_npnq(p, q, r) = tau42_npnq(p, q, r) + ( &
                    N40(p, q) * tau41_npnq(q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau42_npnq(p, q, r) = tau42_npnq(p, q, r) - ( &
                    2 * tau19_npnq(p, q) * z2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau42_npnq(p, q, r) = tau42_npnq(p, q, r) - ( &
                    2 * tau20_npnq(p, q) * z2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau43_npnq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau43_npnq(p) = tau43_npnq(p) + ( &
            N31pq(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau43_npnq(p) = tau43_npnq(p) + ( &
            N31qp(p, p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau44_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau44_npnq(p, q) = tau44_npnq(p, q) + ( &
                N20p(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau44_npnq(p, q) = tau44_npnq(p, q) + ( &
                2*deltaf(p, q) * tau43_npnq(p) &
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
                tau45_npnq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau45_npnq(p, q, r) = tau45_npnq(p, q, r) + ( &
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
                tau46_npnq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau46_npnq(p, q, r) = tau46_npnq(p, q, r) - ( &
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
                tau46_npnq(p, q, r) = tau46_npnq(p, q, r) + ( &
                    2 * tau45_npnq(p, r, q) &
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
                        tau46_npnq(p, q, r) = tau46_npnq(p, q, r) + ( &
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
                    tau47_npnq(p, q, r, s) = 0.0
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
                    tau47_npnq(p, q, r, s) = tau47_npnq(p, q, r, s) - ( &
                        2 * NT22qp(p, q) * z3(r, q, s) &
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
                    tau47_npnq(p, q, r, s) = tau47_npnq(p, q, r, s) - ( &
                        4 * N31qp(p, q) * tau18_npnq(q, r, s) &
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
                    tau47_npnq(p, q, r, s) = tau47_npnq(p, q, r, s) + ( &
                        4 * tau44_npnq(p, q) * tau18_npnq(p, s, r) &
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
                    tau47_npnq(p, q, r, s) = tau47_npnq(p, q, r, s) - ( &
                        N40(q, p) * tau46_npnq(q, s, r) &
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
                    tau48_npnq(p, q, r, s) = 0.0
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
                    tau48_npnq(p, q, r, s) = tau48_npnq(p, q, r, s) + ( &
                        N40(q, p) * tau45_npnq(p, r, s) &
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
                    tau48_npnq(p, q, r, s) = tau48_npnq(p, q, r, s) + ( &
                        2 * N31pq(p, q) * tau18_npnq(p, r, s) &
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
                    tau48_npnq(p, q, r, s) = tau48_npnq(p, q, r, s) - ( &
                        2 * N20q(p, q) * tau18_npnq(q, r, s) &
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
                    tau48_npnq(p, q, r, s) = tau48_npnq(p, q, r, s) + ( &
                        tau36_npnq(p, q) * z3(s, p, r) &
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
                tau49_npnq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau49_npnq(p, q, r) = tau49_npnq(p, q, r) - ( &
                    2 * N31pq(p, q) * z2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau49_npnq(p, q, r) = tau49_npnq(p, q, r) + ( &
                    2 * N40(q, p) * tau39_npnq(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau49_npnq(p, q, r) = tau49_npnq(p, q, r) + ( &
                    N40(p, q) * tau39_npnq(q, r) &
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
                tau50_npnq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau50_npnq(p, q, r) = tau50_npnq(p, q, r) + ( &
                    2 * N31qp(p, q) * z2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau50_npnq(p, q, r) = tau50_npnq(p, q, r) - ( &
                    N40(q, p) * tau39_npnq(p, r) &
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
            tau51_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau51_npnq(p, q) = tau51_npnq(p, q) + ( &
                N22NN(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau51_npnq(p, q) = tau51_npnq(p, q) - ( &
                t1(p) * N31qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau51_npnq(p, q) = tau51_npnq(p, q) - ( &
                t1(q) * tau19_npnq(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau52_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau52_npnq(p, q) = tau52_npnq(p, q) + ( &
                2 * t2(q, p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau52_npnq(p, q) = tau52_npnq(p, q) + ( &
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
            tau53_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau53_npnq(p, q) = tau53_npnq(p, q) + ( &
                N13pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau53_npnq(p, q) = tau53_npnq(p, q) + ( &
                2 * t1(p) * N22NN(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau53_npnq(p, q) = tau53_npnq(p, q) - ( &
                N31qp(p, q) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau54_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau54_npnq(p, q) = tau54_npnq(p, q) + ( &
                NT22pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau54_npnq(p, q) = tau54_npnq(p, q) + ( &
                2 * t1(p) * N31pq(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55_npnq(p, q) = tau55_npnq(p, q) + ( &
                N04(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55_npnq(p, q) = tau55_npnq(p, q) + ( &
                2 * t1(q) * N13qp(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55_npnq(p, q) = tau55_npnq(p, q) - ( &
                NT22qp(q, p) * t1(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55_npnq(p, q) = tau55_npnq(p, q) + ( &
                4 * t2(q, p) * tau51_npnq(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55_npnq(p, q) = tau55_npnq(p, q) + ( &
                N40(q, p) * tau52_npnq(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55_npnq(p, q) = tau55_npnq(p, q) + ( &
                2 * t1(p) * tau53_npnq(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55_npnq(p, q) = tau55_npnq(p, q) - ( &
                tau54_npnq(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau56_npnq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau56_npnq(p) = tau56_npnq(p) + ( &
                t2(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau57_npnq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau57_npnq(p) = tau57_npnq(p) + ( &
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
        tau58_npnq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau58_npnq(p) = tau58_npnq(p) + ( &
            6 * tau56_npnq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau58_npnq(p) = tau58_npnq(p) + ( &
            3 * tau57_npnq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau58_npnq(p) = tau58_npnq(p) + ( &
            tau1_npnq(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau59_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau59_npnq(p, q) = tau59_npnq(p, q) + ( &
                    6 * t2(r, q) * tau39_npnq(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau59_npnq(p, q) = tau59_npnq(p, q) + ( &
                6 * z1(p) * tau2_npnq(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau59_npnq(p, q) = tau59_npnq(p, q) + ( &
                t1(q) * tau58_npnq(p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau60_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau60_npnq(p, q) = tau60_npnq(p, q) + ( &
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
        tau61_npnq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau61_npnq(p) = tau61_npnq(p) + ( &
            2 * tau56_npnq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau61_npnq(p) = tau61_npnq(p) + ( &
            tau57_npnq(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau62_npnq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau62_npnq(p) = tau62_npnq(p) + ( &
                z1(q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau63_npnq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau63_npnq(p) = tau63_npnq(p) + ( &
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
        tau64_npnq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau64_npnq(p) = tau64_npnq(p) + ( &
            2 * tau62_npnq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau64_npnq(p) = tau64_npnq(p) + ( &
            tau63_npnq(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65_npnq(p, q) = tau65_npnq(p, q) + ( &
                2 * tau60_npnq(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65_npnq(p, q) = tau65_npnq(p, q) + ( &
                2 * tau57_npnq(p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65_npnq(p, q) = tau65_npnq(p, q) + ( &
                2 * t1(p) * t1(q) * tau61_npnq(p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65_npnq(p, q) = tau65_npnq(p, q) - ( &
                t1(p) * tau64_npnq(q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau66_npnq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau66_npnq(p) = tau66_npnq(p) + ( &
            t1(p) * z1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau66_npnq(p) = tau66_npnq(p) + ( &
            tau56_npnq(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau67_npnq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau67_npnq(p) = tau67_npnq(p) + ( &
            t1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau67_npnq(p) = tau67_npnq(p) - ( &
            z1(p) * t1(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68_npnq(p, q) = tau68_npnq(p, q) - ( &
                2 * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68_npnq(p, q) = tau68_npnq(p, q) + ( &
                2 * t1(q) * z1(p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau68_npnq(p, q) = tau68_npnq(p, q) - ( &
                    2 * t2(r, q) * tau60_npnq(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68_npnq(p, q) = tau68_npnq(p, q) + ( &
                tau65_npnq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68_npnq(p, q) = tau68_npnq(p, q) + ( &
                tau65_npnq(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68_npnq(p, q) = tau68_npnq(p, q) + ( &
                4 * tau66_npnq(p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68_npnq(p, q) = tau68_npnq(p, q) + ( &
                4 * tau66_npnq(q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68_npnq(p, q) = tau68_npnq(p, q) - ( &
                2 * t1(p) * tau67_npnq(q) &
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
                tau69_npnq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau69_npnq(p, q, r) = tau69_npnq(p, q, r) - ( &
                    NT22qp(p, q) * z2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau69_npnq(p, q, r) = tau69_npnq(p, q, r) + ( &
                    tau39_npnq(p, r) * tau44_npnq(p, q) &
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
                tau70_npnq(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau70_npnq(p, q, r) = tau70_npnq(p, q, r) + ( &
                    NT22pq(p, q) * z2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau70_npnq(p, q, r) = tau70_npnq(p, q, r) - ( &
                    N20q(p, q) * tau39_npnq(q, r) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau71_npnq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau71_npnq(p) = tau71_npnq(p) - ( &
            N22NN(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau71_npnq(p) = tau71_npnq(p) + ( &
            t1(p) * tau43_npnq(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau72_npnq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau72_npnq(p) = tau72_npnq(p) - ( &
            2 * t1(p) * N22NN(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau72_npnq(p) = tau72_npnq(p) + ( &
            tau43_npnq(p) * t1(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau73_npnq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau73_npnq(p) = tau73_npnq(p) + ( &
                        tau71_npnq(p) * t4(s, p, q, r) * z4(q, p, r, s) &
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
                tau73_npnq(p) = tau73_npnq(p) + ( &
                    3 * tau71_npnq(p) * t3(r, p, q) * z3(q, p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau73_npnq(p) = tau73_npnq(p) + ( &
                6 * tau71_npnq(p) * t2(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau73_npnq(p) = tau73_npnq(p) + ( &
            3 * tau72_npnq(p) * z1(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau74_npnq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau74_npnq(p) = tau74_npnq(p) - ( &
            6 * t1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau74_npnq(p) = tau74_npnq(p) + ( &
            6 * z1(p) * t1(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau74_npnq(p) = tau74_npnq(p) - ( &
            6 * tau62_npnq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau74_npnq(p) = tau74_npnq(p) - ( &
            3 * tau63_npnq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau74_npnq(p) = tau74_npnq(p) - ( &
            tau0_npnq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau74_npnq(p) = tau74_npnq(p) + ( &
            2 * t1(p) * tau58_npnq(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau75_npnq(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau75_npnq(p) = tau75_npnq(p) + ( &
            6 * t1(p) * z1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau75_npnq(p) = tau75_npnq(p) + ( &
            6 * tau56_npnq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau75_npnq(p) = tau75_npnq(p) + ( &
            3 * tau57_npnq(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau75_npnq(p) = tau75_npnq(p) + ( &
            tau1_npnq(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau76_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau76_npnq(p, q) = tau76_npnq(p, q) + ( &
                N02p(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau76_npnq(p, q) = tau76_npnq(p, q) + ( &
                t1(q) * NT22pq(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau77_npnq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau77_npnq(p, q) = tau77_npnq(p, q) + ( &
                N02q(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau77_npnq(p, q) = tau77_npnq(p, q) + ( &
                t1(p) * NT22qp(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            NpNq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            NpNq(p, q) = NpNq(p, q) + ( &
                N00(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            NpNq(p, q) = NpNq(p, q) - ( &
                N40(q, p) * tau3_npnq(p, q) / 6 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            NpNq(p, q) = NpNq(p, q) - ( &
                N40(p, q) * tau3_npnq(q, p) / 6 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    NpNq(p, q) = NpNq(p, q) + ( &
                        tau17_npnq(p, q, r, s) * z4(r, p, q, s) &
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
                    NpNq(p, q) = NpNq(p, q) - ( &
                        t4(r, p, q, s) * tau21_npnq(p, q, r, s) / 2 &
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
                        NpNq(p, q) = NpNq(p, q) + ( &
                            t4(r, p, s, p0) * tau24_npnq(p, q, r, s, p0) / 6 &
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
                        NpNq(p, q) = NpNq(p, q) + ( &
                            t4(r, q, s, p0) * tau26_npnq(p, q, r, s, p0) / 6 &
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
                    NpNq(p, q) = NpNq(p, q) - ( &
                        t1(p) * t3(s, r, q) * tau27_npnq(p, q, r, s) &
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
                    NpNq(p, q) = NpNq(p, q) - ( &
                        t1(q) * t3(s, r, p) * tau28_npnq(p, q, r, s) &
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
                    NpNq(p, q) = NpNq(p, q) - ( &
                        tau22_npnq(p, r, q, s) * tau29_npnq(p, q, r, s) &
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
                    NpNq(p, q) = NpNq(p, q) - ( &
                        tau22_npnq(q, r, p, s) * tau30_npnq(p, q, r, s) &
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
                NpNq(p, q) = NpNq(p, q) + ( &
                    2 * tau38_npnq(p, q, r) * z3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                NpNq(p, q) = NpNq(p, q) - ( &
                    t3(r, p, q) * tau42_npnq(p, q, r) &
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
                    NpNq(p, q) = NpNq(p, q) - ( &
                        t3(r, p, s) * tau47_npnq(p, q, r, s) / 4 &
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
                    NpNq(p, q) = NpNq(p, q) + ( &
                        t3(r, q, s) * tau48_npnq(p, q, r, s) / 2 &
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
                NpNq(p, q) = NpNq(p, q) - ( &
                    t1(p) * t2(q, r) * tau49_npnq(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                NpNq(p, q) = NpNq(p, q) + ( &
                    2 * t2(r, q) * tau20_npnq(p, q) * tau45_npnq(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                NpNq(p, q) = NpNq(p, q) + ( &
                    t1(q) * t2(p, r) * tau50_npnq(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            NpNq(p, q) = NpNq(p, q) + ( &
                tau55_npnq(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            NpNq(p, q) = NpNq(p, q) + ( &
                N31pq(p, q) * tau59_npnq(p, q) / 3 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            NpNq(p, q) = NpNq(p, q) - ( &
                N40(q, p) * tau68_npnq(q, p) / 2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                NpNq(p, q) = NpNq(p, q) - ( &
                    t2(r, p) * tau69_npnq(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            NpNq(p, q) = NpNq(p, q) + ( &
                2 * z1(q) * N31qp(p, q) * tau2_npnq(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            NpNq(p, q) = NpNq(p, q) + ( &
                t1(p) * tau58_npnq(q) * N31qp(p, q) / 3 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                NpNq(p, q) = NpNq(p, q) + ( &
                    t2(r, q) * tau70_npnq(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            NpNq(p, q) = NpNq(p, q) - ( &
                2*deltaf(p, q) * tau73_npnq(p) / 3 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            NpNq(p, q) = NpNq(p, q) - ( &
                tau74_npnq(p) * N20p(p, q) / 6 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            NpNq(p, q) = NpNq(p, q) - ( &
                tau74_npnq(q) * N20q(p, q) / 6 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            NpNq(p, q) = NpNq(p, q) + ( &
                tau75_npnq(p) * N11p(p, q) / 3 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            NpNq(p, q) = NpNq(p, q) + ( &
                tau75_npnq(q) * N11q(p, q) / 3 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            NpNq(p, q) = NpNq(p, q) + ( &
                z1(p) * tau76_npnq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            NpNq(p, q) = NpNq(p, q) + ( &
                z1(q) * tau77_npnq(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel
        End Subroutine CCSDTQ_NpNq

pure double precision function deltaf(p, q)
  implicit none
  integer, intent(in) :: p, q

  if (p == q) then
    deltaf = 1.0d0
  else
    deltaf = 0.0d0
  end if

end function deltaf
End Module CCSDTQNpNq
