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
   integer                      :: p,q,r,s,i,j,k,l
   complex(kind=pr), intent(out) :: SpSq(NAO,NAO)
   complex(kind=pr) :: tmp



    complex(kind=pr) , dimension(NAO) :: tau0
    complex(kind=pr) , dimension(NAO) :: tau1
    complex(kind=pr) , dimension(NAO, NAO) :: tau2
    complex(kind=pr) , dimension(NAO, NAO) :: tau3
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau4
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau5
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau6
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau7
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau8
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau9
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau10
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau11
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau12
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau13
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau14
    complex(kind=pr) , dimension(NAO, NAO) :: tau15
    complex(kind=pr) , dimension(NAO, NAO) :: tau16
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau17
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau18
    complex(kind=pr) , dimension(NAO, NAO) :: tau19
    complex(kind=pr) , dimension(NAO, NAO) :: tau20
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau21
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau22
    complex(kind=pr) , dimension(NAO, NAO) :: tau23
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO, NAO) :: tau24
    complex(kind=pr) , dimension(NAO, NAO) :: tau25
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO, NAO) :: tau26
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau27
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau28
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau29
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau30
    complex(kind=pr) , dimension(NAO, NAO) :: tau31
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau32
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau33
    complex(kind=pr) , dimension(NAO, NAO) :: tau34
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau35
    complex(kind=pr) , dimension(NAO, NAO) :: tau36
    complex(kind=pr) , dimension(NAO, NAO) :: tau37
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau38
    complex(kind=pr) , dimension(NAO, NAO) :: tau39
    complex(kind=pr) , dimension(NAO, NAO) :: tau40
    complex(kind=pr) , dimension(NAO, NAO) :: tau41
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau42
    complex(kind=pr) , dimension(NAO) :: tau43
    complex(kind=pr) , dimension(NAO, NAO) :: tau44
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau45
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau46
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau47
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau48
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau49
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau50
    complex(kind=pr) , dimension(NAO, NAO) :: tau51
    complex(kind=pr) , dimension(NAO) :: tau52
    complex(kind=pr) , dimension(NAO) :: tau53
    complex(kind=pr) , dimension(NAO) :: tau54
    complex(kind=pr) , dimension(NAO, NAO) :: tau55
    complex(kind=pr) , dimension(NAO) :: tau56
    complex(kind=pr) , dimension(NAO, NAO) :: tau57
    complex(kind=pr) , dimension(NAO, NAO) :: tau58
    complex(kind=pr) , dimension(NAO) :: tau59
    complex(kind=pr) , dimension(NAO) :: tau60
    complex(kind=pr) , dimension(NAO) :: tau61
    complex(kind=pr) , dimension(NAO, NAO) :: tau62
    complex(kind=pr) , dimension(NAO, NAO) :: tau63
    complex(kind=pr) , dimension(NAO, NAO) :: tau64
    complex(kind=pr) , dimension(NAO, NAO) :: tau65
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau66
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau67
    complex(kind=pr) , dimension(NAO) :: tau68
    complex(kind=pr) , dimension(NAO) :: tau69
    complex(kind=pr) , dimension(NAO) :: tau70
    complex(kind=pr) , dimension(NAO) :: tau71
    complex(kind=pr) , dimension(NAO) :: tau72
    complex(kind=pr) , dimension(NAO, NAO) :: tau73
    complex(kind=pr) , dimension(NAO, NAO) :: tau74
     

    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau0(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau0(p) = tau0(p) + ( &
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
        tau1(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau1(p) = tau1(p) + ( &
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
            tau2(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau2(p, q) = tau2(p, q) + ( &
                t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau2(p, q) = tau2(p, q) + ( &
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
            tau3(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau3(p, q) = tau3(p, q) - ( &
                t1(p) * tau0(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau3(p, q) = tau3(p, q) + ( &
                2 * tau1(p) * tau2(q, p) &
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
                    tau4(p, q, r, s) = 0.0
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
                    tau4(p, q, r, s) = tau4(p, q, r, s) + ( &
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
                    tau4(p, q, r, s) = tau4(p, q, r, s) + ( &
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
                    tau5(p, q, r, s) = 0.0
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
                    tau5(p, q, r, s) = tau5(p, q, r, s) + ( &
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
                    tau5(p, q, r, s) = tau5(p, q, r, s) + ( &
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
                tau6(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau6(p, q, r) = tau6(p, q, r) + ( &
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
                tau6(p, q, r) = tau6(p, q, r) + ( &
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
                tau6(p, q, r) = tau6(p, q, r) + ( &
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
                    tau7(p, q, r, s) = 0.0
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
                    tau7(p, q, r, s) = tau7(p, q, r, s) + ( &
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
                    tau7(p, q, r, s) = tau7(p, q, r, s) + ( &
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
                    tau8(p, q, r, s) = 0.0
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
                    tau8(p, q, r, s) = tau8(p, q, r, s) + ( &
                        tau4(p, q, r, s) &
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
                    tau8(p, q, r, s) = tau8(p, q, r, s) + ( &
                        tau4(q, p, r, s) &
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
                    tau8(p, q, r, s) = tau8(p, q, r, s) + ( &
                        2 * tau2(q, p) * tau5(s, p, q, r) &
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
                    tau8(p, q, r, s) = tau8(p, q, r, s) + ( &
                        2 * t3(s, p, q) * tau6(r, p, q) &
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
                    tau8(p, q, r, s) = tau8(p, q, r, s) + ( &
                        2 * t2(q, p) * tau7(q, s, p, r) &
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
                tau9(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau9(p, q, r) = tau9(p, q, r) + ( &
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
                tau9(p, q, r) = tau9(p, q, r) + ( &
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
                tau9(p, q, r) = tau9(p, q, r) + ( &
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
                    tau10(p, q, r, s) = 0.0
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
                    tau10(p, q, r, s) = tau10(p, q, r, s) + ( &
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
                    tau10(p, q, r, s) = tau10(p, q, r, s) + ( &
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
                    tau10(p, q, r, s) = tau10(p, q, r, s) + ( &
                        2 * t2(q, p) * tau9(s, p, r) &
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
                    tau10(p, q, r, s) = tau10(p, q, r, s) + ( &
                        2 * tau2(r, p) * t3(s, p, q) &
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
                    tau11(p, q, r, s) = 0.0
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
                    tau11(p, q, r, s) = tau11(p, q, r, s) + ( &
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
                    tau11(p, q, r, s) = tau11(p, q, r, s) + ( &
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
                    tau11(p, q, r, s) = tau11(p, q, r, s) + ( &
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
                tau12(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau12(p, q, r) = tau12(p, q, r) + ( &
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
                tau12(p, q, r) = tau12(p, q, r) + ( &
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
                    tau13(p, q, r, s) = 0.0
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
                    tau13(p, q, r, s) = tau13(p, q, r, s) + ( &
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
                    tau13(p, q, r, s) = tau13(p, q, r, s) + ( &
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
                    tau13(p, q, r, s) = tau13(p, q, r, s) + ( &
                        2 * t1(p) * tau11(s, p, q, r) &
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
                    tau13(p, q, r, s) = tau13(p, q, r, s) + ( &
                        2 * t2(q, p) * tau12(s, p, r) &
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
                    tau14(p, q, r, s) = 0.0
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
                    tau14(p, q, r, s) = tau14(p, q, r, s) + ( &
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
                    tau14(p, q, r, s) = tau14(p, q, r, s) + ( &
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
                    tau14(p, q, r, s) = tau14(p, q, r, s) + ( &
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
                    tau14(p, q, r, s) = tau14(p, q, r, s) + ( &
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
            tau15(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau15(p, q) = tau15(p, q) + ( &
                S31pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau15(p, q) = tau15(p, q) - ( &
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
            tau16(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16(p, q) = tau16(p, q) + ( &
                S31qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16(p, q) = tau16(p, q) - ( &
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
                    tau17(p, q, r, s) = 0.0
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
                    tau17(p, q, r, s) = tau17(p, q, r, s) - ( &
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
                    tau17(p, q, r, s) = tau17(p, q, r, s) - ( &
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
                    tau17(p, q, r, s) = tau17(p, q, r, s) + ( &
                        S04(q, p) * tau8(q, p, s, r) &
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
                    tau17(p, q, r, s) = tau17(p, q, r, s) - ( &
                        S13qp(p, q) * tau10(p, s, q, r) &
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
                    tau17(p, q, r, s) = tau17(p, q, r, s) - ( &
                        S13pq(p, q) * tau13(q, s, p, r) &
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
                    tau17(p, q, r, s) = tau17(p, q, r, s) + ( &
                        2 * S22NN(p, q) * tau14(s, p, q, r) &
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
                    tau17(p, q, r, s) = tau17(p, q, r, s) + ( &
                        tau15(p, q) * t3(s, q, r) &
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
                    tau17(p, q, r, s) = tau17(p, q, r, s) + ( &
                        tau16(p, q) * t3(s, p, r) &
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
                tau18(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau18(p, q, r) = tau18(p, q, r) + ( &
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
            tau19(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19(p, q) = tau19(p, q) + ( &
                S13pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19(p, q) = tau19(p, q) - ( &
                t1(p) * S04(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20(p, q) = tau20(p, q) + ( &
                S13qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20(p, q) = tau20(p, q) - ( &
                t1(q) * S04(p, q) &
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
                    tau21(p, q, r, s) = 0.0
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
                    tau21(p, q, r, s) = tau21(p, q, r, s) - ( &
                        S04(p, q) * z2(r, s) &
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
                    tau21(p, q, r, s) = tau21(p, q, r, s) + ( &
                        2 * S04(q, p) * tau18(p, r, s) &
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
                    tau21(p, q, r, s) = tau21(p, q, r, s) + ( &
                        2 * S04(p, q) * tau18(q, r, s) &
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
                    tau21(p, q, r, s) = tau21(p, q, r, s) - ( &
                        2 * tau19(q, p) * z3(s, q, r) &
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
                    tau21(p, q, r, s) = tau21(p, q, r, s) - ( &
                        2 * tau20(q, p) * z3(s, p, r) &
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
                    tau22(p, q, r, s) = 0.0
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
                        tau22(p, q, r, s) = tau22(p, q, r, s) + ( &
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
            tau23(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23(p, q) = tau23(p, q) + ( &
                ST22qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23(p, q) = tau23(p, q) + ( &
                2 * t1(q) * S13qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23(p, q) = tau23(p, q) - ( &
                S04(p, q) * t1(q)**2 &
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
                        tau24(p, q, r, s, p0) = 0.0
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
                        tau24(p, q, r, s, p0) = tau24(p, q, r, s, p0) + ( &
                            S04(p, q) * tau22(q, s, p0, r) &
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
                        tau24(p, q, r, s, p0) = tau24(p, q, r, s, p0) + ( &
                            tau23(p, q) * z4(p0, q, r, s) &
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
            tau25(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau25(p, q) = tau25(p, q) + ( &
                ST22pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau25(p, q) = tau25(p, q) + ( &
                2 * t1(p) * S13pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau25(p, q) = tau25(p, q) - ( &
                S04(q, p) * t1(p)**2 &
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
                        tau26(p, q, r, s, p0) = 0.0
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
                        tau26(p, q, r, s, p0) = tau26(p, q, r, s, p0) + ( &
                            S04(q, p) * tau22(p, s, p0, r) &
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
                        tau26(p, q, r, s, p0) = tau26(p, q, r, s, p0) + ( &
                            tau25(p, q) * z4(p0, p, r, s) &
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
                    tau27(p, q, r, s) = 0.0
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
                    tau27(p, q, r, s) = tau27(p, q, r, s) - ( &
                        S13pq(p, q) * z3(r, p, s) &
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
                    tau27(p, q, r, s) = tau27(p, q, r, s) + ( &
                        S04(q, p) * tau18(p, r, s) &
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
                    tau27(p, q, r, s) = tau27(p, q, r, s) + ( &
                        S04(p, q) * tau18(q, r, s) &
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
                    tau28(p, q, r, s) = 0.0
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
                    tau28(p, q, r, s) = tau28(p, q, r, s) - ( &
                        S13qp(p, q) * z3(r, q, s) &
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
                    tau28(p, q, r, s) = tau28(p, q, r, s) + ( &
                        S04(q, p) * tau18(p, r, s) &
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
                    tau28(p, q, r, s) = tau28(p, q, r, s) + ( &
                        S04(p, q) * tau18(q, r, s) &
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
                    tau29(p, q, r, s) = 0.0
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
                    tau29(p, q, r, s) = tau29(p, q, r, s) + ( &
                        S04(p, q) * t2(r, q) * t2(s, q) &
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
                    tau29(p, q, r, s) = tau29(p, q, r, s) - ( &
                        tau20(p, q) * t3(s, q, r) &
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
                    tau30(p, q, r, s) = 0.0
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
                    tau30(p, q, r, s) = tau30(p, q, r, s) + ( &
                        S04(q, p) * t2(r, p) * t2(s, p) &
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
                    tau30(p, q, r, s) = tau30(p, q, r, s) - ( &
                        tau19(p, q) * t3(s, p, r) &
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
            tau31(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau31(p, q) = tau31(p, q) + ( &
                S22NN(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau31(p, q) = tau31(p, q) + ( &
                S04(p, q) * t2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau31(p, q) = tau31(p, q) - ( &
                t1(p) * S13qp(p, q) &
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
                tau32(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau32(p, q, r) = tau32(p, q, r) + ( &
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
                tau32(p, q, r) = tau32(p, q, r) + ( &
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
                tau33(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau33(p, q, r) = tau33(p, q, r) + ( &
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
                tau33(p, q, r) = tau33(p, q, r) + ( &
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
                tau33(p, q, r) = tau33(p, q, r) + ( &
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
            tau34(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau34(p, q) = tau34(p, q) + ( &
                S22NN(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau34(p, q) = tau34(p, q) - ( &
                t1(q) * S13pq(p, q) &
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
                tau35(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau35(p, q, r) = tau35(p, q, r) + ( &
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
                tau35(p, q, r) = tau35(p, q, r) + ( &
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
            tau36(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau36(p, q) = tau36(p, q) + ( &
                ST22pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau36(p, q) = tau36(p, q) - ( &
                S04(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau37(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau37(p, q) = tau37(p, q) + ( &
                S31pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau37(p, q) = tau37(p, q) - ( &
                t1(q) * tau36(p, q) &
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
                tau38(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38(p, q, r) = tau38(p, q, r) + ( &
                    2 * tau31(p, q) * tau32(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38(p, q, r) = tau38(p, q, r) - ( &
                    tau19(p, q) * tau33(q, r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38(p, q, r) = tau38(p, q, r) + ( &
                    2 * t1(p) * t2(q, r) * tau34(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38(p, q, r) = tau38(p, q, r) - ( &
                    S13qp(p, q) * tau35(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38(p, q, r) = tau38(p, q, r) + ( &
                    t2(r, q) * tau37(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau38(p, q, r) = tau38(p, q, r) + ( &
                    t2(r, p) * tau16(p, q) &
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
            tau39(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau39(p, q) = tau39(p, q) + ( &
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
            tau40(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau40(p, q) = tau40(p, q) + ( &
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
            tau41(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau41(p, q) = tau41(p, q) + ( &
                2 * tau39(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau41(p, q) = tau41(p, q) + ( &
                tau40(p, q) &
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
                tau42(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau42(p, q, r) = tau42(p, q, r) - ( &
                    z1(r) * S04(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau42(p, q, r) = tau42(p, q, r) + ( &
                    S04(q, p) * tau41(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau42(p, q, r) = tau42(p, q, r) + ( &
                    S04(p, q) * tau41(q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau42(p, q, r) = tau42(p, q, r) - ( &
                    2 * tau19(q, p) * z2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau42(p, q, r) = tau42(p, q, r) - ( &
                    2 * tau20(q, p) * z2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau43(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau43(p) = tau43(p) + ( &
            S13pq(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau43(p) = tau43(p) + ( &
            S13qp(p, p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau44(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau44(p, q) = tau44(p, q) + ( &
                S02p(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau44(p, q) = tau44(p, q) + ( &
                2*deltaf(p, q) * tau43(p) &
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
                tau45(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau45(p, q, r) = tau45(p, q, r) + ( &
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
                tau46(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau46(p, q, r) = tau46(p, q, r) - ( &
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
                tau46(p, q, r) = tau46(p, q, r) + ( &
                    2 * tau45(p, r, q) &
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
                        tau46(p, q, r) = tau46(p, q, r) + ( &
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
                    tau47(p, q, r, s) = 0.0
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
                    tau47(p, q, r, s) = tau47(p, q, r, s) - ( &
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
                    tau47(p, q, r, s) = tau47(p, q, r, s) - ( &
                        4 * S13qp(p, q) * tau18(q, r, s) &
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
                    tau47(p, q, r, s) = tau47(p, q, r, s) + ( &
                        4 * tau44(p, q) * tau18(p, s, r) &
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
                    tau47(p, q, r, s) = tau47(p, q, r, s) - ( &
                        S04(q, p) * tau46(q, s, r) &
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
                    tau48(p, q, r, s) = 0.0
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
                    tau48(p, q, r, s) = tau48(p, q, r, s) + ( &
                        S04(q, p) * tau45(p, r, s) &
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
                    tau48(p, q, r, s) = tau48(p, q, r, s) + ( &
                        2 * S13pq(p, q) * tau18(p, r, s) &
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
                    tau48(p, q, r, s) = tau48(p, q, r, s) - ( &
                        2 * S02q(p, q) * tau18(q, r, s) &
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
                    tau48(p, q, r, s) = tau48(p, q, r, s) + ( &
                        tau36(p, q) * z3(s, p, r) &
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
                tau49(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau49(p, q, r) = tau49(p, q, r) - ( &
                    2 * S13pq(p, q) * z2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau49(p, q, r) = tau49(p, q, r) + ( &
                    2 * S04(q, p) * tau39(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau49(p, q, r) = tau49(p, q, r) + ( &
                    S04(p, q) * tau39(q, r) &
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
                tau50(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau50(p, q, r) = tau50(p, q, r) + ( &
                    2 * S13qp(p, q) * z2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau50(p, q, r) = tau50(p, q, r) - ( &
                    S04(q, p) * tau39(p, r) &
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
            tau51(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau51(p, q) = tau51(p, q) + ( &
                2 * t1(p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau51(p, q) = tau51(p, q) + ( &
                t1(q) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau52(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau52(p) = tau52(p) + ( &
                t2(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau53(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau53(p) = tau53(p) + ( &
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
        tau54(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau54(p) = tau54(p) + ( &
            6 * tau52(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau54(p) = tau54(p) + ( &
            3 * tau53(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau54(p) = tau54(p) + ( &
            tau1(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau55(p, q) = tau55(p, q) + ( &
                    6 * t2(r, q) * tau39(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55(p, q) = tau55(p, q) - ( &
                6 * tau51(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55(p, q) = tau55(p, q) + ( &
                6 * z1(p) * tau2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55(p, q) = tau55(p, q) + ( &
                t1(q) * tau54(p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau56(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau56(p) = tau56(p) + ( &
            2 * tau52(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau56(p) = tau56(p) + ( &
            tau53(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau57(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau57(p, q) = tau57(p, q) - ( &
                S04(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau57(p, q) = tau57(p, q) - ( &
                2 * z1(p) * S13qp(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau57(p, q) = tau57(p, q) - ( &
                4 * S22NN(q, p) * z2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau57(p, q) = tau57(p, q) + ( &
                tau56(p) * S04(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau57(p, q) = tau57(p, q) + ( &
                tau56(q) * S04(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau58(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau58(p, q) = tau58(p, q) + ( &
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
        tau59(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau59(p) = tau59(p) + ( &
                z1(q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau60(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau60(p) = tau60(p) + ( &
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
        tau61(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau61(p) = tau61(p) + ( &
            2 * tau59(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau61(p) = tau61(p) + ( &
            tau60(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau62(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau62(p, q) = tau62(p, q) - ( &
                2 * tau58(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau62(p, q) = tau62(p, q) + ( &
                t1(p) * tau61(q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau63(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau63(p, q) = tau63(p, q) + ( &
                2 * t2(q, p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau63(p, q) = tau63(p, q) + ( &
                t1(p)**2 * t1(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau63(p, q) = tau63(p, q) + ( &
                4 * t1(p) * t1(q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau64(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau64(p, q) = tau64(p, q) + ( &
                    2 * t2(r, q) * tau58(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau64(p, q) = tau64(p, q) + ( &
                tau62(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau64(p, q) = tau64(p, q) + ( &
                tau62(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau64(p, q) = tau64(p, q) + ( &
                2 * tau63(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau64(p, q) = tau64(p, q) - ( &
                2 * z1(p) * tau51(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau64(p, q) = tau64(p, q) - ( &
                2 * z1(q) * tau51(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65(p, q) = tau65(p, q) - ( &
                S40(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65(p, q) = tau65(p, q) + ( &
                ST22pq(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65(p, q) = tau65(p, q) + ( &
                ST22qp(q, p) * t1(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65(p, q) = tau65(p, q) - ( &
                2 * t1(p) * S31pq(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65(p, q) = tau65(p, q) - ( &
                2 * t1(q) * S31qp(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65(p, q) = tau65(p, q) + ( &
                2 * S13qp(q, p) * tau51(q, p) &
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
                tau66(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau66(p, q, r) = tau66(p, q, r) - ( &
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
                tau66(p, q, r) = tau66(p, q, r) + ( &
                    tau39(p, r) * tau44(p, q) &
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
                tau67(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau67(p, q, r) = tau67(p, q, r) + ( &
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
                tau67(p, q, r) = tau67(p, q, r) - ( &
                    S02q(p, q) * tau39(q, r) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau68(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau68(p) = tau68(p) - ( &
            S22NN(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau68(p) = tau68(p) + ( &
            t1(p) * tau43(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau69(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau69(p) = tau69(p) - ( &
            2 * t1(p) * S22NN(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau69(p) = tau69(p) + ( &
            tau43(p) * t1(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau70(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau70(p) = tau70(p) + ( &
                        tau68(p) * t4(s, p, q, r) * z4(q, p, r, s) &
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
                tau70(p) = tau70(p) + ( &
                    3 * tau68(p) * t3(r, p, q) * z3(q, p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau70(p) = tau70(p) + ( &
                6 * tau68(p) * t2(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau70(p) = tau70(p) + ( &
            3 * tau69(p) * z1(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau71(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau71(p) = tau71(p) - ( &
            6 * t1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau71(p) = tau71(p) + ( &
            6 * z1(p) * t1(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau71(p) = tau71(p) - ( &
            6 * tau59(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau71(p) = tau71(p) - ( &
            3 * tau60(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau71(p) = tau71(p) - ( &
            tau0(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau71(p) = tau71(p) + ( &
            2 * t1(p) * tau54(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau72(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau72(p) = tau72(p) + ( &
            6 * t1(p) * z1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau72(p) = tau72(p) + ( &
            6 * tau52(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau72(p) = tau72(p) + ( &
            3 * tau53(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau72(p) = tau72(p) + ( &
            tau1(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau73(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau73(p, q) = tau73(p, q) + ( &
                S20p(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau73(p, q) = tau73(p, q) + ( &
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
            tau74(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau74(p, q) = tau74(p, q) + ( &
                S20q(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau74(p, q) = tau74(p, q) + ( &
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
                S04(q, p) * tau3(p, q) / 6 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                S04(p, q) * tau3(q, p) / 6 &
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
                        tau17(p, q, r, s) * z4(r, p, q, s) &
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
                        t4(r, p, q, s) * tau21(q, p, r, s) / 2 &
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
                            t4(r, p, s, p0) * tau24(p, q, r, s, p0) / 6 &
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
                            t4(r, q, s, p0) * tau26(p, q, r, s, p0) / 6 &
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
                        t1(p) * t3(s, r, q) * tau27(p, q, r, s) &
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
                        t1(q) * t3(s, r, p) * tau28(p, q, r, s) &
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
                        tau22(p, r, q, s) * tau29(p, q, r, s) &
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
                        tau22(q, r, p, s) * tau30(p, q, r, s) &
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
                    2 * tau38(p, q, r) * z3(r, p, q) &
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
                    t3(r, p, q) * tau42(q, p, r) &
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
                        t3(r, p, s) * tau47(p, q, r, s) / 4 &
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
                        t3(r, q, s) * tau48(p, q, r, s) / 2 &
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
                    t1(p) * t2(q, r) * tau49(p, q, r) &
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
                    2 * t2(r, q) * tau20(p, q) * tau45(p, r, q) &
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
                    t1(q) * t2(p, r) * tau50(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                S13pq(p, q) * tau55(p, q) / 3 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                tau2(q, p) * tau57(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                S04(q, p) * tau64(q, p) / 2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                tau65(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                SpSq(p, q) = SpSq(p, q) - ( &
                    t2(r, p) * tau66(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                t1(p) * tau54(q) * S13qp(p, q) / 3 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                SpSq(p, q) = SpSq(p, q) + ( &
                    t2(r, q) * tau67(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                2*deltaf(p, q) * tau70(p) / 3 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                tau71(p) * S02p(p, q) / 6 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) - ( &
                tau71(q) * S02q(p, q) / 6 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                tau72(p) * S11p(p, q) / 3 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                tau72(q) * S11q(p, q) / 3 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                z1(p) * tau73(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            SpSq(p, q) = SpSq(p, q) + ( &
                z1(q) * tau74(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel
    do p = 1, NAO
	SpSq(p,p) = 0.75_pr
    do q = p+1, NAO
       tmp = 0.5_pr * (SpSq(p,q) + (SpSq(q,p)))
       SpSq(p,q) = tmp
       SpSq(q,p) = (tmp)
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
