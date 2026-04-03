Module CCSDTQSpSqGrad
    Use Precision
    Use Constants

    Contains

subroutine CCSDTQ_SpSq_Grad(x, &
    T1, T2,T3,T4, &
    Z1, Z2,Z3,Z4, &
    NAO, &
    H00, H20, H11, H02, H40, H31, H22, HT22, H13, H04, &
    S00, S20p, S20q, S11p, S11q, S02p, S02q, &
    S40, S31pq, S31qp, S22NN, ST22pq, ST22qp, S13pq, S13qp, S04, &
    dSpSq)

   use Precision
   implicit none
   integer, intent(in) :: NAO
   complex(kind=pr), intent(in) :: T1(NAO), T2(NAO,NAO),T3(NAO,NAO,NAO), T4(NAO,NAO,NAO,NAO)
   complex(kind=pr), intent(in) :: z1(NAO), z2(NAO,NAO),z3(NAO,NAO,NAO), z4(NAO,NAO,NAO,NAO)
   real(kind=pr) , intent(in)    :: x
   complex(kind=pr), intent(in) :: S00(NAO,NAO)
   complex(kind=pr), intent(in) :: S20p(NAO,NAO), S20q(NAO,NAO)
   complex(kind=pr), intent(in) :: S11p(NAO,NAO), S11q(NAO,NAO)
   complex(kind=pr), intent(in) :: S02p(NAO,NAO), S02q(NAO,NAO)
   complex(kind=pr), intent(in) :: S40(NAO,NAO), S04(NAO,NAO)
   complex(kind=pr), intent(in) :: S31pq(NAO,NAO), S31qp(NAO,NAO)
   complex(kind=pr), intent(in) :: S22NN(NAO,NAO)
   complex(kind=pr), intent(in) :: ST22pq(NAO,NAO), ST22qp(NAO,NAO)
   complex(kind=pr), intent(in) :: S13pq(NAO,NAO), S13qp(NAO,NAO)
   Complex (Kind=pr), Intent(In)    :: H00, H20(NAO), H11(NAO), H02(NAO)
   Complex (Kind=pr), Intent(In)    :: H40(NAO,NAO), H31(NAO,NAO)
   Complex (Kind=pr), Intent(In)    :: H04(NAO,NAO), H13(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H22(NAO,NAO), HT22(NAO,NAO)
   integer                      :: p,p0,q,r,s,i,j,k,l
   complex(kind=pr), intent(out) :: dSpSq(NAO,NAO)
   complex(kind=pr) :: tmp

 

    complex(kind=pr) , dimension(NAO) :: tau0
    complex(kind=pr) , dimension(NAO) :: tau1
    complex(kind=pr) , dimension(NAO, NAO) :: tau2
    complex(kind=pr) , dimension(NAO) :: tau3
    complex(kind=pr) , dimension(NAO) :: tau4
    complex(kind=pr) , dimension(NAO) :: tau5
    complex(kind=pr) , dimension(NAO) :: tau6
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau7
    complex(kind=pr) , dimension(NAO, NAO) :: tau8
    complex(kind=pr) , dimension(NAO) :: tau9
    complex(kind=pr) , dimension(NAO, NAO) :: tau10
    complex(kind=pr) , dimension(NAO, NAO) :: tau11
    complex(kind=pr) , dimension(NAO) :: tau12
    complex(kind=pr) , dimension(NAO) :: tau13
    complex(kind=pr) , dimension(NAO) :: tau14
    complex(kind=pr) , dimension(NAO) :: tau15
    complex(kind=pr) , dimension(NAO) :: tau16
    complex(kind=pr) , dimension(NAO) :: tau17
    complex(kind=pr) , dimension(NAO) :: tau18
    complex(kind=pr) , dimension(NAO, NAO) :: tau19
    complex(kind=pr) , dimension(NAO) :: tau20
    complex(kind=pr) , dimension(NAO, NAO) :: tau21
    complex(kind=pr) , dimension(NAO) :: tau22
    complex(kind=pr) , dimension(NAO, NAO) :: tau23
    complex(kind=pr) , dimension(NAO) :: tau24
    complex(kind=pr) , dimension(NAO) :: tau25
    complex(kind=pr) , dimension(NAO) :: tau26
    complex(kind=pr) , dimension(NAO) :: tau27
    complex(kind=pr) , dimension(NAO) :: tau28
    complex(kind=pr) , dimension(NAO, NAO) :: tau29
    complex(kind=pr) , dimension(NAO) :: tau30
    complex(kind=pr) , dimension(NAO) :: tau31
    complex(kind=pr) , dimension(NAO) :: tau32
    complex(kind=pr) , dimension(NAO) :: tau33
    complex(kind=pr) , dimension(NAO) :: tau34
    complex(kind=pr) , dimension(NAO) :: tau35
    complex(kind=pr) , dimension(NAO) :: tau36
    complex(kind=pr) , dimension(NAO) :: tau37
    complex(kind=pr) , dimension(NAO) :: tau38
    complex(kind=pr) , dimension(NAO) :: tau39
    complex(kind=pr) , dimension(NAO) :: tau40
    complex(kind=pr) , dimension(NAO) :: tau41
    complex(kind=pr) , dimension(NAO, NAO) :: tau42
    complex(kind=pr) , dimension(NAO) :: tau43
    complex(kind=pr) , dimension(NAO) :: tau44
    complex(kind=pr) , dimension(NAO) :: tau45
    complex(kind=pr) , dimension(NAO, NAO) :: tau46
    complex(kind=pr) , dimension(NAO) :: tau47
    complex(kind=pr) , dimension(NAO, NAO) :: tau48
    complex(kind=pr) , dimension(NAO) :: tau49
    complex(kind=pr) , dimension(NAO) :: tau50
    complex(kind=pr) , dimension(NAO) :: tau51
    complex(kind=pr) , dimension(NAO, NAO) :: tau52
    complex(kind=pr) , dimension(NAO) :: tau53
    complex(kind=pr) , dimension(NAO, NAO) :: tau54
    complex(kind=pr) , dimension(NAO) :: tau55
    complex(kind=pr) , dimension(NAO) :: tau56
    complex(kind=pr) , dimension(NAO, NAO) :: tau57
    complex(kind=pr) , dimension(NAO) :: tau58
    complex(kind=pr) , dimension(NAO) :: tau59
    complex(kind=pr) , dimension(NAO) :: tau60
    complex(kind=pr) , dimension(NAO) :: tau61
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau62
    complex(kind=pr) , dimension(NAO, NAO) :: tau63
    complex(kind=pr) , dimension(NAO) :: tau64
    complex(kind=pr) , dimension(NAO, NAO) :: tau65
    complex(kind=pr) , dimension(NAO) :: tau66
    complex(kind=pr) , dimension(NAO, NAO) :: tau67
    complex(kind=pr) , dimension(NAO, NAO) :: tau68
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau69
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau70
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau71
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau72
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau73
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau74
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau75
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau76
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau77
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau78
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau79
    complex(kind=pr) , dimension(NAO, NAO) :: tau80
    complex(kind=pr) , dimension(NAO, NAO) :: tau81
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau82
    complex(kind=pr) , dimension(NAO, NAO) :: tau83
    complex(kind=pr) , dimension(NAO, NAO) :: tau84
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau85
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau86
    complex(kind=pr) , dimension(NAO, NAO) :: tau87
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO, NAO) :: tau88
    complex(kind=pr) , dimension(NAO, NAO) :: tau89
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO, NAO) :: tau90
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau91
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau92
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau93
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau94
    complex(kind=pr) , dimension(NAO, NAO) :: tau95
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau96
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau97
    complex(kind=pr) , dimension(NAO, NAO) :: tau98
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau99
    complex(kind=pr) , dimension(NAO, NAO) :: tau100
    complex(kind=pr) , dimension(NAO, NAO) :: tau101
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau102
    complex(kind=pr) , dimension(NAO, NAO) :: tau103
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau104
    complex(kind=pr) , dimension(NAO) :: tau105
    complex(kind=pr) , dimension(NAO, NAO) :: tau106
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau107
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau108
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau109
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau110
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau111
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau112
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau113
    complex(kind=pr) , dimension(NAO) :: tau114
    complex(kind=pr) , dimension(NAO) :: tau115
    complex(kind=pr) , dimension(NAO, NAO) :: tau116
    complex(kind=pr) , dimension(NAO, NAO) :: tau117
    complex(kind=pr) , dimension(NAO) :: tau118
    complex(kind=pr) , dimension(NAO) :: tau119
    complex(kind=pr) , dimension(NAO, NAO) :: tau120
    complex(kind=pr) , dimension(NAO, NAO) :: tau121
    complex(kind=pr) , dimension(NAO, NAO) :: tau122
    complex(kind=pr) , dimension(NAO, NAO) :: tau123
    complex(kind=pr) , dimension(NAO, NAO) :: tau124
    complex(kind=pr) , dimension(NAO) :: tau125
    complex(kind=pr) , dimension(NAO, NAO) :: tau126
    complex(kind=pr) , dimension(NAO, NAO) :: tau127
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau128
    complex(kind=pr) , dimension(NAO, NAO) :: tau129
    complex(kind=pr) , dimension(NAO) :: tau130
    complex(kind=pr) , dimension(NAO, NAO) :: tau131
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau132
    complex(kind=pr) , dimension(NAO) :: tau133
    complex(kind=pr) , dimension(NAO) :: tau134
    complex(kind=pr) , dimension(NAO) :: tau135
    complex(kind=pr) , dimension(NAO, NAO) :: tau136
    complex(kind=pr) , dimension(NAO) :: tau137
    complex(kind=pr) , dimension(NAO) :: tau138
    complex(kind=pr) , dimension(NAO, NAO) :: tau139
    complex(kind=pr) , dimension(NAO, NAO) :: tau140
    complex(kind=pr) , dimension(NAO, NAO) :: tau141
    complex(kind=pr) , dimension(NAO, NAO) :: tau142
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau143
    complex(kind=pr) , dimension(NAO, NAO) :: tau144
    complex(kind=pr) , dimension(NAO, NAO) :: tau145
    complex(kind=pr) , dimension(NAO, NAO) :: tau146
    complex(kind=pr) , dimension(NAO, NAO) :: tau147
    complex(kind=pr) , dimension(NAO, NAO) :: tau148
    complex(kind=pr) , dimension(NAO, NAO) :: tau149
    complex(kind=pr) , dimension(NAO, NAO) :: tau150
    complex(kind=pr) , dimension(NAO, NAO) :: tau151
    complex(kind=pr) , dimension(NAO, NAO, NAO, NAO) :: tau152
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau153
    complex(kind=pr) , dimension(NAO, NAO) :: tau154
    complex(kind=pr) , dimension(NAO) :: tau155
    complex(kind=pr) , dimension(NAO) :: tau156
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau157
    complex(kind=pr) , dimension(NAO, NAO, NAO) :: tau158
    complex(kind=pr) , dimension(NAO, NAO) :: tau159
    complex(kind=pr) , dimension(NAO) :: tau160
     


    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau0(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau0(p) = tau0(p) + ( &
                H40(q, p) * t2(q, p) &
            )
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
            do r=1, NAO
                do s=1, NAO
                    do p0=1, NAO
                        tau2(p, q) = tau2(p, q) + ( &
                            t4(p0, p, r, s) * z4(p0, q, r, s) &
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
        tau3(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau3(p) = tau3(p) + ( &
                H31(p, q) * tau2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau4(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau4(p) = tau4(p) + ( &
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
        tau5(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau5(p) = tau5(p) + ( &
                tau4(q) * H40(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau6(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau6(p) = tau6(p) + ( &
                t1(q) * H31(p, q) &
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
                tau7(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau7(p, q, r) = tau7(p, q, r) + ( &
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
            tau8(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau8(p, q) = tau8(p, q) + ( &
                        t3(s, q, r) * tau7(p, s, r) &
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
        tau9(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau9(p) = tau9(p) + ( &
                H40(q, p) * tau8(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau10(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau10(p, q) = tau10(p, q) + ( &
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
            tau11(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau11(p, q) = tau11(p, q) + ( &
                    t2(r, p) * tau10(q, r) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau12(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau12(p) = tau12(p) + ( &
                H40(q, p) * tau11(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau13(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau13(p) = tau13(p) + ( &
                t2(q, p) * tau10(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau14(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau14(p) = tau14(p) + ( &
                tau13(q) * H40(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau15(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau15(p) = tau15(p) + ( &
                t1(q) * H40(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau16(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau16(p) = tau16(p) + ( &
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
        tau17(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau17(p) = tau17(p) + ( &
                    H40(r, q) * t3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau18(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau18(p) = tau18(p) + ( &
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
        do q=1, NAO
            tau19(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau19(p, q) = tau19(p, q) + ( &
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
        tau20(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau20(p) = tau20(p) + ( &
                t2(q, p) * tau19(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau21(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau21(p, q) = tau21(p, q) + ( &
                    z2(r, p) * t3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau22(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau22(p) = tau22(p) + ( &
                H40(q, p) * tau21(p, q) &
            )
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
            do r=1, NAO
                tau23(p, q) = tau23(p, q) + ( &
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
        tau24(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau24(p) = tau24(p) + ( &
                H31(q, p) * tau23(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau25(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau25(p) = tau25(p) + ( &
                t2(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau26(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau26(p) = tau26(p) + ( &
                HT22(q, p) * tau19(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau27(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau27(p) = tau27(p) + ( &
                z1(q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau28(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau28(p) = tau28(p) + ( &
                tau27(q) * H40(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau29(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau29(p, q) = tau29(p, q) + ( &
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
        tau30(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau30(p) = tau30(p) + ( &
                H31(p, q) * tau29(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau31(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau31(p) = tau31(p) + ( &
                H22(p, q) * tau19(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau32(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau32(p) = tau32(p) + ( &
                H22(q, p) * tau19(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau33(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau33(p) = tau33(p) + ( &
                t1(q) * H31(p, q) * tau19(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau34(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau34(p) = tau34(p) + ( &
                t1(q) * H40(p, q) * tau23(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau35(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau35(p) = tau35(p) + ( &
                H40(q, p) * t2(p, q) * tau19(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau36(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau36(p) = tau36(p) + ( &
                t1(q) * HT22(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau37(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau37(p) = tau37(p) + ( &
                H13(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau38(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau38(p) = tau38(p) + ( &
                H31(p, q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau39(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau39(p) = tau39(p) + ( &
                H31(q, p) * t2(p, q) * z2(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau40(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau40(p) = tau40(p) + ( &
                t1(q) * H22(p, q) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau41(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau41(p) = tau41(p) + ( &
                t1(q) * H40(p, q) * t2(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau42(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau42(p, q) = tau42(p, q) + ( &
                        t3(s, p, r) * z3(s, q, r) &
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
        tau43(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau43(p) = tau43(p) + ( &
                H31(p, q) * tau42(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau44(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau44(p) = tau44(p) + ( &
                tau18(q) * H40(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau45(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau45(p) = tau45(p) + ( &
                HT22(q, p) * tau10(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau46(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau46(p, q) = tau46(p, q) + ( &
                        z3(s, p, r) * t4(s, p, q, r) &
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
        tau47(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau47(p) = tau47(p) + ( &
                H40(q, p) * tau46(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau48(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau48(p, q) = tau48(p, q) + ( &
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
        tau49(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau49(p) = tau49(p) + ( &
                H31(q, p) * tau48(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau50(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau50(p) = tau50(p) + ( &
                H22(p, q) * tau10(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau51(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau51(p) = tau51(p) + ( &
                H22(q, p) * tau10(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau52(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau52(p, q) = tau52(p, q) + ( &
                    t2(r, q) * tau19(p, r) &
                )
            end do
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
            tau53(p) = tau53(p) + ( &
                H40(q, p) * tau52(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau54(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau54(p, q) = tau54(p, q) + ( &
                        H31(r, p) * t2(s, r) * z4(s, p, q, r) &
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
        tau55(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau55(p) = tau55(p) + ( &
                t2(q, p) * tau54(p, q) &
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
        do q=1, NAO
            tau56(p) = tau56(p) + ( &
                t1(q) * H31(p, q) * tau10(q, p) &
            )
        end do
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
            do r=1, NAO
                do s=1, NAO
                    tau57(p, q) = tau57(p, q) + ( &
                        t2(r, p) * t2(s, p) * z4(s, p, q, r) &
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
        tau58(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau58(p) = tau58(p) + ( &
                H31(p, q) * tau57(q, p) &
            )
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
                tau20(q) * H40(q, p) &
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
            tau60(p) = tau60(p) + ( &
                t1(q) * H40(q, p) * tau48(q, p) &
            )
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
        do q=1, NAO
            tau61(p) = tau61(p) + ( &
                H40(q, p) * t2(p, q) * tau10(q, p) &
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
                tau62(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau62(p, q, r) = tau62(p, q, r) + ( &
                        t3(s, p, q) * z4(s, p, q, r) &
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
            tau63(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau63(p, q) = tau63(p, q) + ( &
                    t2(r, p) * tau62(q, p, r) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau64(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau64(p) = tau64(p) + ( &
                H40(q, p) * tau63(q, p) &
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
            do r=1, NAO
                do s=1, NAO
                    tau65(p, q) = tau65(p, q) + ( &
                        t1(r) * H40(p, r) * t2(s, r) * z4(s, p, q, r) &
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
        tau66(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau66(p) = tau66(p) + ( &
                t2(q, p) * tau65(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau67(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau67(p, q) = tau67(p, q) + ( &
                t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau67(p, q) = tau67(p, q) + ( &
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
            tau68(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68(p, q) = tau68(p, q) - ( &
                t1(p) * tau4(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68(p, q) = tau68(p, q) + ( &
                2 * tau1(p) * tau67(q, p) &
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
                    tau69(p, q, r, s) = 0.0
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
                    tau69(p, q, r, s) = tau69(p, q, r, s) + ( &
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
                    tau69(p, q, r, s) = tau69(p, q, r, s) + ( &
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
                    tau70(p, q, r, s) = 0.0
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
                    tau70(p, q, r, s) = tau70(p, q, r, s) + ( &
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
                    tau70(p, q, r, s) = tau70(p, q, r, s) + ( &
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
                tau71(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau71(p, q, r) = tau71(p, q, r) + ( &
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
                tau71(p, q, r) = tau71(p, q, r) + ( &
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
                tau71(p, q, r) = tau71(p, q, r) + ( &
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
                    tau72(p, q, r, s) = 0.0
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
                    tau72(p, q, r, s) = tau72(p, q, r, s) + ( &
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
                    tau72(p, q, r, s) = tau72(p, q, r, s) + ( &
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
                    tau73(p, q, r, s) = 0.0
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
                    tau73(p, q, r, s) = tau73(p, q, r, s) + ( &
                        tau69(p, q, r, s) &
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
                    tau73(p, q, r, s) = tau73(p, q, r, s) + ( &
                        tau69(q, p, r, s) &
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
                    tau73(p, q, r, s) = tau73(p, q, r, s) + ( &
                        2 * tau67(q, p) * tau70(s, p, q, r) &
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
                    tau73(p, q, r, s) = tau73(p, q, r, s) + ( &
                        2 * t3(s, p, q) * tau71(r, p, q) &
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
                    tau73(p, q, r, s) = tau73(p, q, r, s) + ( &
                        2 * t2(q, p) * tau72(q, s, p, r) &
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
                tau74(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau74(p, q, r) = tau74(p, q, r) + ( &
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
                tau74(p, q, r) = tau74(p, q, r) + ( &
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
                tau74(p, q, r) = tau74(p, q, r) + ( &
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
                    tau75(p, q, r, s) = 0.0
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
                    tau75(p, q, r, s) = tau75(p, q, r, s) + ( &
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
                    tau75(p, q, r, s) = tau75(p, q, r, s) + ( &
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
                    tau75(p, q, r, s) = tau75(p, q, r, s) + ( &
                        2 * t2(q, p) * tau74(s, p, r) &
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
                    tau75(p, q, r, s) = tau75(p, q, r, s) + ( &
                        2 * tau67(r, p) * t3(s, p, q) &
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
                    tau76(p, q, r, s) = 0.0
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
                    tau76(p, q, r, s) = tau76(p, q, r, s) + ( &
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
                    tau76(p, q, r, s) = tau76(p, q, r, s) + ( &
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
                    tau76(p, q, r, s) = tau76(p, q, r, s) + ( &
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
                tau77(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau77(p, q, r) = tau77(p, q, r) + ( &
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
                tau77(p, q, r) = tau77(p, q, r) + ( &
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
                    tau78(p, q, r, s) = 0.0
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
                    tau78(p, q, r, s) = tau78(p, q, r, s) + ( &
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
                    tau78(p, q, r, s) = tau78(p, q, r, s) + ( &
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
                    tau78(p, q, r, s) = tau78(p, q, r, s) + ( &
                        2 * t1(p) * tau76(s, p, q, r) &
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
                    tau78(p, q, r, s) = tau78(p, q, r, s) + ( &
                        2 * t2(q, p) * tau77(s, p, r) &
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
                    tau79(p, q, r, s) = 0.0
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
                    tau79(p, q, r, s) = tau79(p, q, r, s) + ( &
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
                    tau79(p, q, r, s) = tau79(p, q, r, s) + ( &
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
                    tau79(p, q, r, s) = tau79(p, q, r, s) + ( &
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
                    tau79(p, q, r, s) = tau79(p, q, r, s) + ( &
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
            tau80(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau80(p, q) = tau80(p, q) + ( &
                S31pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau80(p, q) = tau80(p, q) - ( &
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
            tau81(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau81(p, q) = tau81(p, q) + ( &
                S31qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau81(p, q) = tau81(p, q) - ( &
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
                    tau82(p, q, r, s) = 0.0
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
                    tau82(p, q, r, s) = tau82(p, q, r, s) - ( &
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
                    tau82(p, q, r, s) = tau82(p, q, r, s) - ( &
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
                    tau82(p, q, r, s) = tau82(p, q, r, s) + ( &
                        S04(q, p) * tau73(q, p, s, r) &
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
                    tau82(p, q, r, s) = tau82(p, q, r, s) - ( &
                        S13qp(p, q) * tau75(p, s, q, r) &
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
                    tau82(p, q, r, s) = tau82(p, q, r, s) - ( &
                        S13pq(p, q) * tau78(q, s, p, r) &
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
                    tau82(p, q, r, s) = tau82(p, q, r, s) + ( &
                        2 * S22NN(p, q) * tau79(s, p, q, r) &
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
                    tau82(p, q, r, s) = tau82(p, q, r, s) + ( &
                        tau80(p, q) * t3(s, q, r) &
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
                    tau82(p, q, r, s) = tau82(p, q, r, s) + ( &
                        tau81(p, q) * t3(s, p, r) &
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
            tau83(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau83(p, q) = tau83(p, q) + ( &
                S13pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau83(p, q) = tau83(p, q) - ( &
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
            tau84(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau84(p, q) = tau84(p, q) + ( &
                S13qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau84(p, q) = tau84(p, q) - ( &
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
                    tau85(p, q, r, s) = 0.0
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
                    tau85(p, q, r, s) = tau85(p, q, r, s) - ( &
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
                    tau85(p, q, r, s) = tau85(p, q, r, s) + ( &
                        2 * S04(q, p) * tau7(p, r, s) &
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
                    tau85(p, q, r, s) = tau85(p, q, r, s) + ( &
                        2 * S04(p, q) * tau7(q, r, s) &
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
                    tau85(p, q, r, s) = tau85(p, q, r, s) - ( &
                        2 * tau83(q, p) * z3(s, q, r) &
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
                    tau85(p, q, r, s) = tau85(p, q, r, s) - ( &
                        2 * tau84(q, p) * z3(s, p, r) &
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
                    tau86(p, q, r, s) = 0.0
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
                        tau86(p, q, r, s) = tau86(p, q, r, s) + ( &
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
            tau87(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau87(p, q) = tau87(p, q) + ( &
                ST22qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau87(p, q) = tau87(p, q) + ( &
                2 * t1(q) * S13qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau87(p, q) = tau87(p, q) - ( &
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
                        tau88(p, q, r, s, p0) = 0.0
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
                        tau88(p, q, r, s, p0) = tau88(p, q, r, s, p0) + ( &
                            S04(p, q) * tau86(q, s, p0, r) &
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
                        tau88(p, q, r, s, p0) = tau88(p, q, r, s, p0) + ( &
                            tau87(p, q) * z4(p0, q, r, s) &
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
            tau89(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau89(p, q) = tau89(p, q) + ( &
                ST22pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau89(p, q) = tau89(p, q) + ( &
                2 * t1(p) * S13pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau89(p, q) = tau89(p, q) - ( &
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
                        tau90(p, q, r, s, p0) = 0.0
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
                        tau90(p, q, r, s, p0) = tau90(p, q, r, s, p0) + ( &
                            S04(q, p) * tau86(p, s, p0, r) &
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
                        tau90(p, q, r, s, p0) = tau90(p, q, r, s, p0) + ( &
                            tau89(p, q) * z4(p0, p, r, s) &
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
                    tau91(p, q, r, s) = 0.0
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
                    tau91(p, q, r, s) = tau91(p, q, r, s) - ( &
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
                    tau91(p, q, r, s) = tau91(p, q, r, s) + ( &
                        S04(q, p) * tau7(p, r, s) &
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
                    tau91(p, q, r, s) = tau91(p, q, r, s) + ( &
                        S04(p, q) * tau7(q, r, s) &
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
                    tau92(p, q, r, s) = 0.0
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
                    tau92(p, q, r, s) = tau92(p, q, r, s) - ( &
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
                    tau92(p, q, r, s) = tau92(p, q, r, s) + ( &
                        S04(q, p) * tau7(p, r, s) &
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
                    tau92(p, q, r, s) = tau92(p, q, r, s) + ( &
                        S04(p, q) * tau7(q, r, s) &
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
                    tau93(p, q, r, s) = 0.0
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
                    tau93(p, q, r, s) = tau93(p, q, r, s) + ( &
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
                    tau93(p, q, r, s) = tau93(p, q, r, s) - ( &
                        tau84(p, q) * t3(s, q, r) &
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
                    tau94(p, q, r, s) = 0.0
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
                    tau94(p, q, r, s) = tau94(p, q, r, s) + ( &
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
                    tau94(p, q, r, s) = tau94(p, q, r, s) - ( &
                        tau83(p, q) * t3(s, p, r) &
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
            tau95(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau95(p, q) = tau95(p, q) + ( &
                S22NN(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau95(p, q) = tau95(p, q) + ( &
                S04(p, q) * t2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau95(p, q) = tau95(p, q) - ( &
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
                tau96(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau96(p, q, r) = tau96(p, q, r) + ( &
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
                tau96(p, q, r) = tau96(p, q, r) + ( &
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
                tau97(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau97(p, q, r) = tau97(p, q, r) + ( &
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
                tau97(p, q, r) = tau97(p, q, r) + ( &
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
                tau97(p, q, r) = tau97(p, q, r) + ( &
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
            tau98(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau98(p, q) = tau98(p, q) + ( &
                S22NN(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau98(p, q) = tau98(p, q) - ( &
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
                tau99(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau99(p, q, r) = tau99(p, q, r) + ( &
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
                tau99(p, q, r) = tau99(p, q, r) + ( &
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
            tau100(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau100(p, q) = tau100(p, q) + ( &
                ST22pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau100(p, q) = tau100(p, q) - ( &
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
            tau101(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau101(p, q) = tau101(p, q) + ( &
                S31pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau101(p, q) = tau101(p, q) - ( &
                t1(q) * tau100(p, q) &
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
                tau102(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau102(p, q, r) = tau102(p, q, r) + ( &
                    2 * tau95(p, q) * tau96(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau102(p, q, r) = tau102(p, q, r) - ( &
                    tau83(p, q) * tau97(q, r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau102(p, q, r) = tau102(p, q, r) + ( &
                    2 * t1(p) * t2(q, r) * tau98(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau102(p, q, r) = tau102(p, q, r) - ( &
                    S13qp(p, q) * tau99(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau102(p, q, r) = tau102(p, q, r) + ( &
                    t2(r, q) * tau101(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau102(p, q, r) = tau102(p, q, r) + ( &
                    t2(r, p) * tau81(p, q) &
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
            tau103(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau103(p, q) = tau103(p, q) + ( &
                2 * tau19(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau103(p, q) = tau103(p, q) + ( &
                tau10(p, q) &
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
                tau104(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau104(p, q, r) = tau104(p, q, r) - ( &
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
                tau104(p, q, r) = tau104(p, q, r) + ( &
                    S04(q, p) * tau103(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau104(p, q, r) = tau104(p, q, r) + ( &
                    S04(p, q) * tau103(q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau104(p, q, r) = tau104(p, q, r) - ( &
                    2 * tau83(q, p) * z2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau104(p, q, r) = tau104(p, q, r) - ( &
                    2 * tau84(q, p) * z2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau105(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau105(p) = tau105(p) + ( &
            S13pq(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau105(p) = tau105(p) + ( &
            S13qp(p, p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau106(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau106(p, q) = tau106(p, q) + ( &
                S02p(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau106(p, q) = tau106(p, q) + ( &
                2*deltaf(p, q) * tau105(p) &
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
                tau107(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau107(p, q, r) = tau107(p, q, r) + ( &
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
                tau108(p, q, r) = 0.0
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
                        tau108(p, q, r) = tau108(p, q, r) + ( &
                            t3(p0, p, s) * z4(p0, q, r, s) &
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
                tau109(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau109(p, q, r) = tau109(p, q, r) - ( &
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
                tau109(p, q, r) = tau109(p, q, r) + ( &
                    2 * tau107(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau109(p, q, r) = tau109(p, q, r) + ( &
                    tau108(p, r, q) &
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
                    tau110(p, q, r, s) = 0.0
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
                    tau110(p, q, r, s) = tau110(p, q, r, s) - ( &
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
                    tau110(p, q, r, s) = tau110(p, q, r, s) - ( &
                        4 * S13qp(p, q) * tau7(q, r, s) &
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
                    tau110(p, q, r, s) = tau110(p, q, r, s) + ( &
                        4 * tau106(p, q) * tau7(p, s, r) &
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
                    tau110(p, q, r, s) = tau110(p, q, r, s) - ( &
                        S04(q, p) * tau109(q, s, r) &
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
                    tau111(p, q, r, s) = 0.0
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
                    tau111(p, q, r, s) = tau111(p, q, r, s) + ( &
                        S04(q, p) * tau107(p, r, s) &
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
                    tau111(p, q, r, s) = tau111(p, q, r, s) + ( &
                        2 * S13pq(p, q) * tau7(p, r, s) &
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
                    tau111(p, q, r, s) = tau111(p, q, r, s) - ( &
                        2 * S02q(p, q) * tau7(q, r, s) &
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
                    tau111(p, q, r, s) = tau111(p, q, r, s) + ( &
                        tau100(p, q) * z3(s, p, r) &
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
                tau112(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau112(p, q, r) = tau112(p, q, r) - ( &
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
                tau112(p, q, r) = tau112(p, q, r) + ( &
                    2 * S04(q, p) * tau19(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau112(p, q, r) = tau112(p, q, r) + ( &
                    S04(p, q) * tau19(q, r) &
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
                tau113(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau113(p, q, r) = tau113(p, q, r) + ( &
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
                tau113(p, q, r) = tau113(p, q, r) - ( &
                    S04(q, p) * tau19(p, r) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau114(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau114(p) = tau114(p) + ( &
            2 * tau25(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau114(p) = tau114(p) + ( &
            tau16(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau115(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau115(p) = tau115(p) + ( &
            2 * tau27(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau115(p) = tau115(p) + ( &
            tau18(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau116(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau116(p, q) = tau116(p, q) + ( &
                2 * tau29(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau116(p, q) = tau116(p, q) + ( &
                2 * tau16(p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau116(p, q) = tau116(p, q) + ( &
                2 * t1(p) * t1(q) * tau114(p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau116(p, q) = tau116(p, q) - ( &
                t1(p) * tau115(q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau117(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau117(p, q) = tau117(p, q) + ( &
                2 * t2(q, p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau117(p, q) = tau117(p, q) + ( &
                t1(p)**2 * t1(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau117(p, q) = tau117(p, q) + ( &
                4 * t1(p) * t1(q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau118(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau118(p) = tau118(p) + ( &
            t1(p) * z1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau118(p) = tau118(p) + ( &
            tau25(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau119(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau119(p) = tau119(p) + ( &
            t1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau119(p) = tau119(p) - ( &
            z1(p) * t1(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau120(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau120(p, q) = tau120(p, q) - ( &
                2 * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau120(p, q) = tau120(p, q) + ( &
                2 * t1(q) * z1(p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau120(p, q) = tau120(p, q) - ( &
                    2 * t2(r, q) * tau29(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau120(p, q) = tau120(p, q) + ( &
                tau116(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau120(p, q) = tau120(p, q) + ( &
                tau116(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau120(p, q) = tau120(p, q) - ( &
                2 * tau117(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau120(p, q) = tau120(p, q) + ( &
                4 * tau118(p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau120(p, q) = tau120(p, q) + ( &
                4 * tau118(q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau120(p, q) = tau120(p, q) - ( &
                2 * t1(p) * tau119(q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau121(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau121(p, q) = tau121(p, q) + ( &
                t1(q) * S13pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau121(p, q) = tau121(p, q) + ( &
                t1(p) * S13qp(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau121(p, q) = tau121(p, q) - ( &
                S22NN(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau122(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau122(p, q) = tau122(p, q) + ( &
                S31pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau122(p, q) = tau122(p, q) + ( &
                2 * t1(p) * S22NN(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau122(p, q) = tau122(p, q) - ( &
                S13qp(p, q) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau123(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau123(p, q) = tau123(p, q) + ( &
                ST22pq(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau123(p, q) = tau123(p, q) + ( &
                2 * t1(p) * S13pq(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau124(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau124(p, q) = tau124(p, q) - ( &
                S40(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau124(p, q) = tau124(p, q) + ( &
                ST22qp(q, p) * t1(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau124(p, q) = tau124(p, q) - ( &
                2 * t1(q) * S31qp(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau124(p, q) = tau124(p, q) + ( &
                4 * t2(q, p) * tau121(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau124(p, q) = tau124(p, q) - ( &
                2 * t1(p) * tau122(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau124(p, q) = tau124(p, q) + ( &
                tau123(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau125(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau125(p) = tau125(p) + ( &
            6 * tau25(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau125(p) = tau125(p) + ( &
            3 * tau16(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau125(p) = tau125(p) + ( &
            tau1(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau126(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau126(p, q) = tau126(p, q) + ( &
                6 * tau52(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau126(p, q) = tau126(p, q) + ( &
                6 * z1(p) * tau67(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau126(p, q) = tau126(p, q) + ( &
                t1(q) * tau125(p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau127(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau127(p, q) = tau127(p, q) + ( &
                6 * z1(p) * tau67(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau127(p, q) = tau127(p, q) + ( &
                t1(q) * tau125(p) &
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
                tau128(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau128(p, q, r) = tau128(p, q, r) - ( &
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
                tau128(p, q, r) = tau128(p, q, r) + ( &
                    tau106(p, q) * tau19(p, r) &
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
            tau129(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau129(p, q) = tau129(p, q) + ( &
                t1(p) * z1(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau129(p, q) = tau129(p, q) + ( &
                tau29(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau130(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau130(p) = tau130(p) - ( &
            S22NN(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau130(p) = tau130(p) + ( &
            t1(p) * tau105(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau131(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau131(p, q) = tau131(p, q) + ( &
                t1(p) * S02p(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau131(p, q) = tau131(p, q) - ( &
                S11p(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau131(p, q) = tau131(p, q) + ( &
                2*deltaf(p, q) * tau130(p) &
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
                tau132(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau132(p, q, r) = tau132(p, q, r) + ( &
                    t1(p) * t3(q, p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau132(p, q, r) = tau132(p, q, r) + ( &
                    t2(q, p) * t2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau133(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau133(p) = tau133(p) + ( &
            6 * tau25(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau133(p) = tau133(p) + ( &
            tau1(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau134(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau134(p) = tau134(p) - ( &
            6 * t1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau134(p) = tau134(p) + ( &
            6 * z1(p) * t1(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau134(p) = tau134(p) - ( &
            6 * tau27(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau134(p) = tau134(p) - ( &
            3 * tau18(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau134(p) = tau134(p) - ( &
            tau4(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau134(p) = tau134(p) + ( &
                    6 * tau132(p, q, r) * z3(q, p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau134(p) = tau134(p) + ( &
            2 * t1(p) * tau133(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau135(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau135(p) = tau135(p) - ( &
            2 * t1(p) * S22NN(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau135(p) = tau135(p) + ( &
            tau105(p) * t1(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau136(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau136(p, q) = tau136(p, q) + ( &
                S20p(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau136(p, q) = tau136(p, q) + ( &
                2 * t1(p) * S11p(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau136(p, q) = tau136(p, q) - ( &
                S02p(p, q) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau136(p, q) = tau136(p, q) - ( &
                2*deltaf(p, q) * tau135(p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau137(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau137(p) = tau137(p) + ( &
            6 * t1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau137(p) = tau137(p) + ( &
            6 * tau27(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau137(p) = tau137(p) + ( &
            3 * tau18(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau137(p) = tau137(p) + ( &
            tau4(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau138(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau138(p) = tau138(p) + ( &
            6 * t1(p) * z1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau138(p) = tau138(p) + ( &
            6 * tau25(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau138(p) = tau138(p) + ( &
            3 * tau16(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau138(p) = tau138(p) + ( &
            tau1(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau139(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau139(p, q) = tau139(p, q) + ( &
                S20q(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau139(p, q) = tau139(p, q) + ( &
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
            tau140(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau140(p, q) = tau140(p, q) + ( &
                    H40(r, p) * t2(r, q) &
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
            tau141(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau141(p, q) = tau141(p, q) + ( &
                HT22(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau141(p, q) = tau141(p, q) + ( &
                2 * tau140(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau142(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau142(p, q) = tau142(p, q) + ( &
                H22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau142(p, q) = tau142(p, q) + ( &
                H40(p, q) * t2(p, q) &
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
                tau143(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau143(p, q, r) = tau143(p, q, r) + ( &
                    4 * H31(r, p) * t3(q, r, p) &
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
                    tau143(p, q, r) = tau143(p, q, r) - ( &
                        H31(p, s) * t3(s, r, q) &
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
                    tau143(p, q, r) = tau143(p, q, r) + ( &
                        2 * H40(s, p) * t4(s, p, r, q) &
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
                tau143(p, q, r) = tau143(p, q, r) + ( &
                    t2(q, p) * tau141(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau143(p, q, r) = tau143(p, q, r) - ( &
                    4 * t2(r, q) * tau142(r, p) &
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
            tau144(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau144(p, q) = tau144(p, q) - ( &
                H13(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau144(p, q) = tau144(p, q) + ( &
                2 * H31(q, p) * t2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau144(p, q) = tau144(p, q) - ( &
                    H31(p, r) * t2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau144(p, q) = tau144(p, q) + ( &
                    2 * H40(r, p) * t3(r, p, q) &
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
            tau145(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau145(p, q) = tau145(p, q) + ( &
                3 * tau29(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau145(p, q) = tau145(p, q) + ( &
                3 * tau42(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau145(p, q) = tau145(p, q) + ( &
                tau2(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau146(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau146(p, q) = tau146(p, q) + ( &
                3 * H40(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau146(p, q) = tau146(p, q) + ( &
                12 * H22(p, q) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau146(p, q) = tau146(p, q) + ( &
                    3 * HT22(r, p) * z2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau146(p, q) = tau146(p, q) + ( &
                12 * H40(p, q) * tau23(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau146(p, q) = tau146(p, q) + ( &
                6 * H40(q, p) * tau48(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau146(p, q) = tau146(p, q) - ( &
                6 * H31(p, q) * tau10(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau146(p, q) = tau146(p, q) + ( &
                    3 * H31(r, p) * tau10(r, q) &
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
                    tau146(p, q) = tau146(p, q) - ( &
                        3 * tau143(p, r, s) * z4(r, p, q, s) &
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
                tau146(p, q) = tau146(p, q) - ( &
                    6 * tau144(p, r) * z3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau146(p, q) = tau146(p, q) + ( &
                    H40(r, p) * tau145(r, q) &
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
            tau147(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau147(p, q) = tau147(p, q) + ( &
                8 * z2(p, q) * t2(p, q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau147(p, q) = tau147(p, q) + ( &
                        2 * z2(s, r) * t4(s, q, p, r) &
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
                tau147(p, q) = tau147(p, q) + ( &
                    8 * t3(r, p, q) * tau62(q, p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau147(p, q) = tau147(p, q) - ( &
                    8 * tau10(q, r) * t3(r, q, p) &
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
                    tau147(p, q) = tau147(p, q) + ( &
                        t3(s, q, r) * tau108(p, s, r) &
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
            tau148(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau148(p, q) = tau148(p, q) + ( &
                3 * tau42(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau148(p, q) = tau148(p, q) + ( &
                tau2(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau149(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau149(p, q) = tau149(p, q) + ( &
                2 * tau23(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau149(p, q) = tau149(p, q) + ( &
                tau48(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau150(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau150(p, q) = tau150(p, q) + ( &
                2 * tau21(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau150(p, q) = tau150(p, q) + ( &
                tau46(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau151(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau151(p, q) = tau151(p, q) - ( &
                H31(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau151(p, q) = tau151(p, q) + ( &
                2 * t1(p) * H40(q, p) &
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
                    tau152(p, q, r, s) = 0.0
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
                    tau152(p, q, r, s) = tau152(p, q, r, s) + ( &
                        3 * H40(q, q) * t4(r, q, s, p) &
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
                    tau152(p, q, r, s) = tau152(p, q, r, s) + ( &
                        6 * H40(q, p) * t2(r, p) * t2(s, p) &
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
                        tau152(p, q, r, s) = tau152(p, q, r, s) - ( &
                            H40(p0, q) * t4(p0, s, p, r) &
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
                    tau152(p, q, r, s) = tau152(p, q, r, s) + ( &
                        3 * tau151(p, q) * t3(s, p, r) &
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
                tau153(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau153(p, q, r) = tau153(p, q, r) + ( &
                    3 * H40(q, q) * t3(p, q, r) &
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
                    tau153(p, q, r) = tau153(p, q, r) - ( &
                        H40(s, q) * t3(s, r, p) &
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
                tau153(p, q, r) = tau153(p, q, r) + ( &
                    2 * t2(r, p) * tau151(p, q) &
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
            tau154(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau154(p, q) = tau154(p, q) + ( &
                HT22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau154(p, q) = tau154(p, q) + ( &
                2 * t1(p) * H31(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau154(p, q) = tau154(p, q) - ( &
                H40(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau154(p, q) = tau154(p, q) - ( &
                6 * H40(q, q) * t2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau154(p, q) = tau154(p, q) + ( &
                2 * tau140(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau155(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau155(p) = tau155(p) + ( &
            H20(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau155(p) = tau155(p) + ( &
            2 * H31(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau155(p) = tau155(p) + ( &
            2 * tau15(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau156(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau156(p) = tau156(p) + ( &
            3 * H40(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau156(p) = tau156(p) - ( &
                        tau152(q, p, r, s) * z4(r, p, q, s) &
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
                tau156(p) = tau156(p) - ( &
                    3 * tau153(q, p, r) * z3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau156(p) = tau156(p) + ( &
                3 * tau154(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau156(p) = tau156(p) + ( &
            3 * tau155(p) * z1(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau157(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau157(p, q, r) = tau157(p, q, r) + ( &
                    4 * t1(p) * z2(q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau157(p, q, r) = tau157(p, q, r) + ( &
                    4 * tau107(p, r, q) &
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
                    tau157(p, q, r) = tau157(p, q, r) - ( &
                        24 * t1(p) * t2(s, p) * z4(s, p, r, q) &
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
                tau157(p, q, r) = tau157(p, q, r) + ( &
                    tau108(p, r, q) &
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
                tau158(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau158(p, q, r) = tau158(p, q, r) + ( &
                    3 * t1(p) * z3(q, p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau158(p, q, r) = tau158(p, q, r) + ( &
                    tau7(p, r, q) &
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
            tau159(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau159(p, q) = tau159(p, q) + ( &
                6 * t1(p) * z1(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau159(p, q) = tau159(p, q) + ( &
                3 * tau29(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau159(p, q) = tau159(p, q) + ( &
                tau2(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau160(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau160(p) = tau160(p) - ( &
            24 * z1(p) * t1(p)**3 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau160(p) = tau160(p) + ( &
            4 * t1(p) * tau4(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau160(p) = tau160(p) + ( &
                    3 * t3(q, p, r) * tau157(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau160(p) = tau160(p) - ( &
                    24 * t2(q, p) * t2(r, p) * tau158(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau160(p) = tau160(p) + ( &
                4 * t2(q, p) * tau159(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                H00 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    H02(r) * z1(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    H20(r) * t1(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2 * H11(r) * t1(r) * z1(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    4 * t1(r) * z1(r) * H22(r, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                x * S00(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * tau0(r) * tau1(r) / 3.0 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    t1(r) * tau3(r) / 3.0 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    t1(r) * tau5(r) / 3.0 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    tau1(r) * tau6(r) / 3 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * t1(r) * tau9(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * t1(r) * tau12(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * t1(r) * tau14(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * t1(r) * tau1(r) * tau15(r) / 3.0 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    H11(r) * tau16(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    tau17(r) * z1(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    H20(r) * tau18(r) / 2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2 * tau16(r) * H22(r, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    H20(r) * t1(r) * tau16(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    H20(r) * tau20(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    4 * t1(r) * tau22(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    4 * t1(r) * tau24(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    4 * tau0(r) * tau25(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * t1(r) * tau16(r) * H31(r, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * t1(r) * tau26(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * tau20(r) * H31(r, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2 * t1(r) * tau28(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2 * t1(r) * tau30(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2 * tau25(r) * tau6(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    4 * t1(r) * tau31(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    4 * t1(r) * tau32(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    4 * t1(r) * tau33(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    4 * t1(r) * tau15(r) * tau25(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    4 * t1(r) * tau34(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    8 * t1(r) * tau35(r) &
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
                    dSpSq(p, q) = dSpSq(p, q) + ( &
                        H04(s, r) * z2(s, r) &
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
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    H20(r) * tau27(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    t1(r) * tau15(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    tau36(r) * z1(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2 * H11(r) * tau25(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2 * t1(r) * tau37(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2 * tau38(r) * z1(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    4 * tau25(r) * H22(r, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    4 * t1(r) * tau0(r) * z1(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    4 * t1(r) * tau25(r) * H31(r, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    4 * t1(r) * tau39(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * H20(r) * t1(r) * tau25(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2 * t1(r) * tau6(r) * z1(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    4 * t1(r) * tau40(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    4 * t1(r) * tau41(r) &
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
                    dSpSq(p, q) = dSpSq(p, q) + ( &
                        H13(r, s) * tau10(s, r) &
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
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    H11(r) * tau1(r) / 3.0 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    H20(r) * tau4(r) / 6.0 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2 * tau1(r) * H22(r, r) / 3.0 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    t1(r) * tau43(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    t1(r) * tau44(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    tau16(r) * tau6(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    H20(r) * tau13(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    t1(r) * tau45(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * t1(r) * tau47(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * t1(r) * tau49(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * tau13(r) * H31(r, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * tau0(r) * tau16(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2 * t1(r) * tau50(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2 * t1(r) * tau51(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * t1(r) * tau1(r) * H31(r, r) / 3.0 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    H20(r) * t1(r) * tau1(r) / 3 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    4 * t1(r) * tau53(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    4 * t1(r) * tau55(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * t1(r) * tau56(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * t1(r) * tau15(r) * tau16(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * t1(r) * tau58(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    2 * t1(r) * tau59(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2 * t1(r) * tau60(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    4 * t1(r) * tau61(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    8 * t1(r) * tau64(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    4 * t1(r) * tau66(r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) - ( &
                x * S04(q, p) * tau68(p, q) / 6.0 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) - ( &
                x * S04(p, q) * tau68(q, p) / 6 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    dSpSq(p, q) = dSpSq(p, q) + ( &
                        x * tau82(p, q, r, s) * z4(r, p, q, s) &
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
                    dSpSq(p, q) = dSpSq(p, q) - ( &
                        x * t4(r, p, q, s) * tau85(q, p, r, s) / 2.0 &
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
                        dSpSq(p, q) = dSpSq(p, q) + ( &
                            x * t4(r, p, s, p0) * tau88(p, q, r, s, p0) / 6.0 &
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
                        dSpSq(p, q) = dSpSq(p, q) + ( &
                            x * t4(r, q, s, p0) * tau90(p, q, r, s, p0) / 6 &
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
                    dSpSq(p, q) = dSpSq(p, q) - ( &
                        x * t1(p) * t3(s, r, q) * tau91(p, q, r, s) &
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
                    dSpSq(p, q) = dSpSq(p, q) - ( &
                        x * t1(q) * t3(s, r, p) * tau92(p, q, r, s) &
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
                    dSpSq(p, q) = dSpSq(p, q) - ( &
                        x * tau86(p, r, q, s) * tau93(p, q, r, s) &
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
                    dSpSq(p, q) = dSpSq(p, q) - ( &
                        x * tau86(q, r, p, s) * tau94(p, q, r, s) &
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
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2*x * tau102(p, q, r) * z3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    x * t3(r, p, q) * tau104(q, p, r) &
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
                    dSpSq(p, q) = dSpSq(p, q) - ( &
                        x * t3(r, p, s) * tau110(p, q, r, s) / 4.0 &
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
                    dSpSq(p, q) = dSpSq(p, q) + ( &
                        x * t3(r, q, s) * tau111(p, q, r, s) / 2 &
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
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    x * t1(p) * t2(q, r) * tau112(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    2*x * t2(r, q) * tau84(p, q) * tau107(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) + ( &
                    x * t1(q) * t2(p, r) * tau113(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) - ( &
                x * S04(q, p) * tau120(q, p) / 2.0 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) - ( &
                x * tau124(q, p) * z2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                x * S13pq(p, q) * tau126(p, q) / 3.0 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                x * S13qp(p, q) * tau127(q, p) / 3.0 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    x * t2(r, p) * tau128(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                x * ST22pq(p, q) * tau129(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) - ( &
                x * tau125(p) * tau131(p, q) / 3.0 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) - ( &
                x * tau134(q) * S02q(p, q) / 6.0 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                x * z1(p) * tau136(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                x * tau137(p) * S02p(p, q) / 6.0 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                x * tau138(q) * S11q(p, q) / 3.0 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            dSpSq(p, q) = dSpSq(p, q) + ( &
                x * z1(q) * tau139(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    dSpSq(p, q) = dSpSq(p, q) + ( &
                        t2(q, p) * tau146(p, q) / 3.0 &
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
                    dSpSq(p, q) = dSpSq(p, q) + ( &
                        H40(p, q) * tau147(p, q) / 4.0 &
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
                    dSpSq(p, q) = dSpSq(p, q) + ( &
                        HT22(q, p) * tau148(p, q) / 6.0 &
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
                    dSpSq(p, q) = dSpSq(p, q) + ( &
                        2 * H22(q, p) * tau149(p, q) &
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
                    dSpSq(p, q) = dSpSq(p, q) + ( &
                        H31(p, q) * tau150(p, q) &
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
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    tau156(p) * t1(p)**2.0 / 3.0 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                dSpSq(p, q) = dSpSq(p, q) - ( &
                    tau160(p) * H40(p, p) / 12.0 &
                )
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel
    do p = 1, NAO
	do q = p+1, NAO
       tmp = 0.5_pr * (dSpSq(p,q) + (dSpSq(q,p)))
       dSpSq(p,q) = tmp
       dSpSq(q,p) = (tmp)
    end do
    end do
End Subroutine CCSDTQ_SpSq_Grad

double precision function deltaf(p, q)
   implicit none
   integer, intent(in) :: p, q

   if (p == q) then
    deltaf = 1.0d0
   else
    deltaf = 0.0d0
   end if

   end function deltaf
End Module CCSDTQSpSqGrad
