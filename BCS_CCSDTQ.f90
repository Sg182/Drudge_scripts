
Module CCResCCSDTQ

    Use Precision
    Use Constants
  
    Contains

Subroutine CCSDTQ(Ene,Res1,Res2,Res3,Res4,T1,T2,T3,T4,NAO, &
        H20,H11,H02,H40,H31,H22,HT22,H13,H04)
    Implicit None
    Integer,           Intent(In)    :: NAO
    Complex (Kind=pr), Intent(In)    :: T1(NAO), T2(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: T3(NAO,NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: T4(NAO,NAO,NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H20(NAO), H11(NAO), H02(NAO)
    Complex (Kind=pr), Intent(In)    :: H40(NAO,NAO), H31(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H04(NAO,NAO), H13(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H22(NAO,NAO), HT22(NAO,NAO)
    Complex (Kind=pr), Intent(Out)   :: Ene
    Complex (Kind=pr), Intent(Out)   :: Res1(NAO), Res2(NAO,NAO)
    Complex (Kind=pr), Intent(Out)   :: Res3(NAO,NAO,NAO)
    Complex (Kind=pr), Intent(Out)   :: Res4(NAO,NAO,NAO,NAO)
    Integer                          :: p, q, r, s, i, j, k, l
    


    complex(kind=pr), dimension(NAO, NAO) :: tau0
    complex(kind=pr), dimension(NAO, NAO) :: tau1
    complex(kind=pr), dimension(NAO) :: tau2
    complex(kind=pr), dimension(NAO) :: tau3
    complex(kind=pr), dimension(NAO) :: tau4
    complex(kind=pr), dimension(NAO) :: tau5
    complex(kind=pr), dimension(NAO) :: tau6
    complex(kind=pr), dimension(NAO, NAO) :: tau7
    complex(kind=pr), dimension(NAO, NAO) :: tau8
    complex(kind=pr), dimension(NAO, NAO) :: tau9
    complex(kind=pr), dimension(NAO, NAO) :: tau10
    complex(kind=pr), dimension(NAO, NAO) :: tau11
    complex(kind=pr), dimension(NAO, NAO) :: tau12
    complex(kind=pr), dimension(NAO, NAO) :: tau13
    complex(kind=pr), dimension(NAO, NAO) :: tau14
    complex(kind=pr), dimension(NAO, NAO) :: tau15
    complex(kind=pr), dimension(NAO, NAO) :: tau16
    complex(kind=pr), dimension(NAO) :: tau17
    complex(kind=pr), dimension(NAO) :: tau18
    complex(kind=pr), dimension(NAO) :: tau19
    complex(kind=pr), dimension(NAO) :: tau20
    complex(kind=pr), dimension(NAO, NAO) :: tau21
    complex(kind=pr), dimension(NAO, NAO) :: tau22
    complex(kind=pr), dimension(NAO, NAO) :: tau23
    complex(kind=pr), dimension(NAO, NAO) :: tau24
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau25
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau26
    complex(kind=pr), dimension(NAO, NAO) :: tau27
    complex(kind=pr), dimension(NAO, NAO) :: tau28
    complex(kind=pr), dimension(NAO, NAO) :: tau29
    complex(kind=pr), dimension(NAO, NAO) :: tau30
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau31
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau32
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau33
    complex(kind=pr), dimension(NAO) :: tau34
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau35
    complex(kind=pr), dimension(NAO, NAO) :: tau36
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau37
    complex(kind=pr), dimension(NAO, NAO) :: tau38
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau39
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau40
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau41
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau42
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau43
    complex(kind=pr), dimension(NAO, NAO) :: tau44
    complex(kind=pr), dimension(NAO) :: tau45
    complex(kind=pr), dimension(NAO) :: tau46
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau47
    complex(kind=pr), dimension(NAO, NAO) :: tau48
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau49
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau50
    complex(kind=pr), dimension(NAO, NAO) :: tau51
    complex(kind=pr), dimension(NAO) :: tau52
    complex(kind=pr), dimension(NAO, NAO) :: tau53
    complex(kind=pr), dimension(NAO) :: tau54
    complex(kind=pr), dimension(NAO) :: tau55
    complex(kind=pr), dimension(NAO, NAO) :: tau56
    complex(kind=pr), dimension(NAO, NAO) :: tau57
    complex(kind=pr), dimension(NAO, NAO) :: tau58
    complex(kind=pr), dimension(NAO, NAO) :: tau59
    complex(kind=pr), dimension(NAO, NAO) :: tau60
    complex(kind=pr), dimension(NAO, NAO) :: tau61
    complex(kind=pr), dimension(NAO) :: tau62
    complex(kind=pr), dimension(NAO, NAO) :: tau63
    complex(kind=pr), dimension(NAO, NAO) :: tau64
    complex(kind=pr), dimension(NAO, NAO) :: tau65
    complex(kind=pr), dimension(NAO, NAO) :: tau66
    complex(kind=pr), dimension(NAO, NAO) :: tau67
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau68
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau69
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau70
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau71
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau72
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau73
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau74
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau75
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau76
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau77
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau78
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau79
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau80
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau81
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau82
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau83
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau84
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau85
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau86
    complex(kind=pr), dimension(NAO, NAO) :: tau87
    complex(kind=pr), dimension(NAO, NAO) :: tau88
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau89
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau90
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau91
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau92
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau93
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau94
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau95
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau96
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau97
    complex(kind=pr), dimension(NAO, NAO) :: tau98
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau99
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau100
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau101
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau102
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau103
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau104
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau105
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau106
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau107
    complex(kind=pr), dimension(NAO) :: tau108
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau109
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau110
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau111
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau112
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau113
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau114
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau115
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau116
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau117
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau118
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau119
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau120
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau121
    complex(kind=pr), dimension(NAO, NAO) :: tau122
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau123
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau124
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau125
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau126
    complex(kind=pr), dimension(NAO, NAO) :: tau127
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau128
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau129
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau130
    complex(kind=pr), dimension(NAO, NAO) :: tau131
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau132
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau133
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau134
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau135
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau136
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau137
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau138
    complex(kind=pr), dimension(NAO, NAO) :: tau139
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau140
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau141
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau142
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau143
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau144
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau145
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau146
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau147
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau148
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau149
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau150
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau151
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau152
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau153
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau154
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau155
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau156
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau157
    complex(kind=pr), dimension(NAO, NAO) :: tau158
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau159
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau160
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau161
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau162
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau163
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau164
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau165
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau166
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau167
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau168
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau169
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau170
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau171
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau172
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau173
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau174
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau175
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau176
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau177
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau178
    complex(kind=pr), dimension(NAO, NAO) :: tau179
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau180
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau181
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau182
    complex(kind=pr), dimension(NAO, NAO) :: tau183
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau184
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau185
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau186
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau187
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau188
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau189
    complex(kind=pr), dimension(NAO, NAO) :: tau190
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau191
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau192
    complex(kind=pr), dimension(NAO, NAO) :: tau193
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau194
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau195
    complex(kind=pr), dimension(NAO, NAO) :: tau196
    complex(kind=pr), dimension(NAO, NAO, NAO, NAO) :: tau197
    complex(kind=pr), dimension(NAO, NAO) :: tau198
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau199
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau200
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau201
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau202
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau203
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau204
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau205
    complex(kind=pr), dimension(NAO, NAO, NAO) :: tau206
    complex(kind=pr), dimension(NAO, NAO) :: tau207
    complex(kind=pr), dimension(NAO, NAO) :: tau208
    complex(kind=pr) :: Ene
    complex(kind=pr), dimension(NAO) :: Res1
    complex(kind=pr), dimension(NAO,NAO) :: Res2
    complex(kind=pr), dimension(NAO,NAO,NAO) :: Res3
    complex(kind=pr), dimension(NAO,NAO,NAO,NAO) :: Res4


    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau0(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau0(p, q) = tau0(p, q) + ( &
                t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau0(p, q) = tau0(p, q) + ( &
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
            tau1(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau1(p, q) = tau1(p, q) - ( &
                H31(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau1(p, q) = tau1(p, q) + ( &
                2 * t1(p) * H40(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau2(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau2(p) = tau2(p) + ( &
                t2(q, p) * tau1(p, q) &
            )
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
                t1(q) * H31(p, q) &
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
        tau4(p) = tau4(p) + ( &
            H11(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau4(p) = tau4(p) + ( &
            2 * H22(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau4(p) = tau4(p) + ( &
            tau3(p) &
        )
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
                t1(q) * H40(q, p) &
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
        tau6(p) = tau6(p) + ( &
            H20(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau6(p) = tau6(p) + ( &
            2 * tau5(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau7(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau7(p, q) = tau7(p, q) - ( &
                HT22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau7(p, q) = tau7(p, q) + ( &
                2 * H40(q, p) * t1(p)**2 &
            )
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
            tau8(p, q) = tau8(p, q) + ( &
                t1(p) * H31(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau8(p, q) = tau8(p, q) - ( &
                H40(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau9(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau9(p, q) = tau9(p, q) + ( &
                    t2(r, p) * tau8(q, r) &
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
            tau10(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau10(p, q) = tau10(p, q) - ( &
                t1(p) * H31(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau10(p, q) = tau10(p, q) + ( &
                H22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau10(p, q) = tau10(p, q) + ( &
                H22(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau10(p, q) = tau10(p, q) + ( &
                t1(q) * tau1(p, q) &
            )
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
            tau11(p, q) = tau11(p, q) + ( &
                2 * t2(q, p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau11(p, q) = tau11(p, q) + ( &
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
            tau12(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau12(p, q) = tau12(p, q) + ( &
                H22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau12(p, q) = tau12(p, q) + ( &
                H22(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau13(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau13(p, q) = tau13(p, q) + ( &
                    tau1(p, r) * t3(r, p, q) &
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
            tau14(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau14(p, q) = tau14(p, q) + ( &
                    tau1(q, r) * t3(r, p, q) &
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
            tau15(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau15(p, q) = tau15(p, q) + ( &
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
            tau16(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16(p, q) = tau16(p, q) + ( &
                HT22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16(p, q) = tau16(p, q) + ( &
                2 * t1(p) * H31(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16(p, q) = tau16(p, q) - ( &
                2 * H40(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau16(p, q) = tau16(p, q) + ( &
                tau15(q, p) &
            )
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
        tau17(p) = tau17(p) + ( &
            4 * H22(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau17(p) = tau17(p) - ( &
            HT22(p, p) &
        )
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
        tau18(p) = tau18(p) + ( &
            2 * t1(p) * H13(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau18(p) = tau18(p) - ( &
            2 * H31(p, p) * t1(p)**3 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau18(p) = tau18(p) + ( &
                t2(q, p) * tau16(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau18(p) = tau18(p) + ( &
            tau17(p) * t1(p)**2 &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau19(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau19(p) = tau19(p) + ( &
                H40(q, p) * tau0(q, p) &
            )
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
        tau20(p) = tau20(p) + ( &
            H20(p) * t1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau20(p) = tau20(p) - ( &
            H11(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau20(p) = tau20(p) + ( &
            2 * t1(p) * H31(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau20(p) = tau20(p) - ( &
            2 * H22(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau20(p) = tau20(p) - ( &
            tau3(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau20(p) = tau20(p) + ( &
            2 * tau19(p) &
        )
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
            tau21(p, q) = tau21(p, q) + ( &
                H13(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau21(p, q) = tau21(p, q) - ( &
                H31(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau22(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau22(p, q) = tau22(p, q) + ( &
                HT22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau22(p, q) = tau22(p, q) + ( &
                2 * t1(p) * H31(p, q) &
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
            tau23(p, q) = tau23(p, q) + ( &
                HT22(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau23(p, q) = tau23(p, q) + ( &
                2 * tau15(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau24(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau24(p, q) = tau24(p, q) + ( &
                HT22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau24(p, q) = tau24(p, q) + ( &
                2 * t1(p) * H31(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau24(p, q) = tau24(p, q) - ( &
                2 * H40(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau24(p, q) = tau24(p, q) + ( &
                2 * tau15(q, p) &
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
                tau25(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau25(p, q, r) = tau25(p, q, r) + ( &
                        tau24(r, s) * t3(s, p, q) &
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
                tau26(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau26(p, q, r) = tau26(p, q, r) + ( &
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
                tau26(p, q, r) = tau26(p, q, r) + ( &
                    t1(q) * t2(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau26(p, q, r) = tau26(p, q, r) + ( &
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
                tau26(p, q, r) = tau26(p, q, r) - ( &
                    deltaf(p, q) * t1(r) * t1(q)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau26(p, q, r) = tau26(p, q, r) - ( &
                    deltaf(p, r) * t1(q) * t1(r)**2 &
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
            tau27(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau27(p, q) = tau27(p, q) + ( &
                HT22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau27(p, q) = tau27(p, q) + ( &
                2 * t1(p) * H31(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau27(p, q) = tau27(p, q) + ( &
                2 * tau15(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau28(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau28(p, q) = tau28(p, q) + ( &
                    H31(p, r) * t2(r, q) &
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
            tau29(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau29(p, q) = tau29(p, q) + ( &
                    H40(r, p) * t3(r, p, q) &
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
            tau30(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau30(p, q) = tau30(p, q) + ( &
                H13(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau30(p, q) = tau30(p, q) - ( &
                H31(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau30(p, q) = tau30(p, q) + ( &
                tau28(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau30(p, q) = tau30(p, q) - ( &
                2 * tau29(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau30(p, q) = tau30(p, q) + ( &
                2 * t1(p) * tau12(q, p) &
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
                tau31(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau31(p, q, r) = tau31(p, q, r) - ( &
                    H31(p, q) * t2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau31(p, q, r) = tau31(p, q, r) - ( &
                    H31(q, p) * t2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau31(p, q, r) = tau31(p, q, r) + ( &
                    2 * H40(q, p) * tau26(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau31(p, q, r) = tau31(p, q, r) + ( &
                    deltaf(q, r) * t1(p) * tau27(q, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau31(p, q, r) = tau31(p, q, r) + ( &
                    deltaf(p, r) * t1(q) * tau27(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau31(p, q, r) = tau31(p, q, r) - ( &
                    deltaf(q, r) * tau30(q, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau31(p, q, r) = tau31(p, q, r) - ( &
                    deltaf(p, r) * tau30(p, q) &
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
                    t1(q) * t2(p, r) &
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
                tau33(p, q, r) = tau33(p, q, r) - ( &
                    H31(p, q) * t2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau33(p, q, r) = tau33(p, q, r) - ( &
                    H31(q, p) * t2(r, p) &
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
                    deltaf(q, r) * t1(p) * tau24(q, p) &
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
                    2 * H40(q, p) * tau32(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau33(p, q, r) = tau33(p, q, r) - ( &
                    deltaf(q, r) * tau30(q, p) &
                )
            end do
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
        tau34(p) = tau34(p) + ( &
            H20(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau34(p) = tau34(p) + ( &
            2 * H31(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau34(p) = tau34(p) + ( &
            2 * tau5(p) &
        )
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
                tau35(p, q, r) = tau35(p, q, r) - ( &
                    4*deltaf(p, q) * tau1(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau35(p, q, r) = tau35(p, q, r) - ( &
                    4*deltaf(q, r) * tau1(r, p) &
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
                    deltaf(p, q) * tau34(r) &
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
                    deltaf(q, r) * tau34(p) &
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
                2 * H40(p, q) * t2(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau36(p, q) = tau36(p, q) + ( &
                H22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau36(p, q) = tau36(p, q) + ( &
                H22(q, p) &
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
                tau37(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau37(p, q, r) = tau37(p, q, r) + ( &
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
                tau37(p, q, r) = tau37(p, q, r) + ( &
                    t1(p) * t2(q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau37(p, q, r) = tau37(p, q, r) + ( &
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
            tau38(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau38(p, q) = tau38(p, q) - ( &
                t1(q) * H31(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau38(p, q) = tau38(p, q) + ( &
                H22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau38(p, q) = tau38(p, q) + ( &
                H22(q, p) &
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
                tau39(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau39(p, q, r) = tau39(p, q, r) + ( &
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
                tau39(p, q, r) = tau39(p, q, r) + ( &
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
                tau40(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau40(p, q, r) = tau40(p, q, r) + ( &
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
                tau40(p, q, r) = tau40(p, q, r) + ( &
                    t1(p) * t2(q, r) &
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
                tau41(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau41(p, q, r) = tau41(p, q, r) + ( &
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
                tau41(p, q, r) = tau41(p, q, r) + ( &
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
                tau41(p, q, r) = tau41(p, q, r) - ( &
                    4*deltaf(q, r) * t2(p, r)**2 &
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
                tau42(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau42(p, q, r) = tau42(p, q, r) + ( &
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
                tau42(p, q, r) = tau42(p, q, r) + ( &
                    t2(q, r) * t1(p)**2 &
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
                tau43(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau43(p, q, r) = tau43(p, q, r) + ( &
                    H31(p, q) * t2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau43(p, q, r) = tau43(p, q, r) + ( &
                    H31(q, p) * t2(r, p) &
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
            tau44(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau44(p, q) = tau44(p, q) + ( &
                    H31(p, r) * t3(r, p, q) &
                )
            end do
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
                H40(q, p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau46(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau46(p) = tau46(p) + ( &
            HT22(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau46(p) = tau46(p) - ( &
            4 * H22(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau46(p) = tau46(p) + ( &
            2 * tau45(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau47(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau47(p, q, r) = tau47(p, q, r) + ( &
                    deltaf(p, q) * t2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau47(p, q, r) = tau47(p, q, r) + ( &
                    deltaf(p, r) * t2(q, p) &
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
            tau48(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau48(p, q) = tau48(p, q) + ( &
                HT22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau48(p, q) = tau48(p, q) + ( &
                2 * tau15(q, p) &
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
                tau49(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau49(p, q, r) = tau49(p, q, r) + ( &
                    2*deltaf(p, q) * tau44(p, r) &
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
                    2*deltaf(p, r) * tau44(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau49(p, q, r) = tau49(p, q, r) - ( &
                    2 * tau46(p) * tau47(p, r, q) &
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
                    tau34(p) * t3(r, p, q) &
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
                    t2(r, p) * tau48(q, p) &
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
                    t2(q, p) * tau23(p, r) &
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
                tau50(p, q, r) = tau50(p, q, r) + ( &
                    t1(q) * t2(p, r) &
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
            do r=1, NAO
                tau51(p, q) = tau51(p, q) + ( &
                    H40(r, p) * tau50(r, p, q) &
                )
            end do
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
        tau52(p) = tau52(p) - ( &
            H13(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau52(p) = tau52(p) + ( &
            3 * H31(p, p) * t1(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau52(p) = tau52(p) + ( &
            tau2(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau52(p) = tau52(p) - ( &
            t1(p) * tau17(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau53(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau53(p, q) = tau53(p, q) + ( &
                t1(p) * HT22(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau53(p, q) = tau53(p, q) - ( &
                H13(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau53(p, q) = tau53(p, q) - ( &
                tau28(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau53(p, q) = tau53(p, q) + ( &
                2 * tau51(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau53(p, q) = tau53(p, q) - ( &
                2*deltaf(p, q) * tau52(p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau53(p, q) = tau53(p, q) + ( &
                tau34(p) * t2(q, p) &
            )
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
        do q=1, NAO
            tau54(p) = tau54(p) + ( &
                H31(p, q) * t2(q, p) &
            )
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
        tau55(p) = tau55(p) + ( &
            H13(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau55(p) = tau55(p) - ( &
            3 * H31(p, p) * t1(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau55(p) = tau55(p) + ( &
            tau54(p) &
        )
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau56(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau56(p, q) = tau56(p, q) - ( &
                H13(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau56(p, q) = tau56(p, q) - ( &
                tau28(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau56(p, q) = tau56(p, q) + ( &
                2 * tau29(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau56(p, q) = tau56(p, q) + ( &
                2*deltaf(p, q) * tau55(q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau56(p, q) = tau56(p, q) + ( &
                tau34(q) * t2(q, p) &
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
            tau57(p, q) = tau57(p, q) - ( &
                H13(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau57(p, q) = tau57(p, q) - ( &
                tau28(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau57(p, q) = tau57(p, q) + ( &
                2 * tau51(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau57(p, q) = tau57(p, q) + ( &
                tau34(p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau57(p, q) = tau57(p, q) - ( &
                t1(p) * tau7(q, p) &
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
            tau58(p, q) = tau58(p, q) - ( &
                H13(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau58(p, q) = tau58(p, q) - ( &
                tau28(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau58(p, q) = tau58(p, q) + ( &
                2 * tau29(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau58(p, q) = tau58(p, q) + ( &
                2*deltaf(p, q) * tau55(p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau59(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau59(p, q) = tau59(p, q) + ( &
                    H40(r, p) * tau39(r, q, p) &
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
            tau60(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau60(p, q) = tau60(p, q) + ( &
                t1(q) * HT22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau60(p, q) = tau60(p, q) - ( &
                H13(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau60(p, q) = tau60(p, q) - ( &
                tau28(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau60(p, q) = tau60(p, q) + ( &
                2 * tau59(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau61(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau61(p, q) = tau61(p, q) + ( &
                t1(p) * HT22(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau61(p, q) = tau61(p, q) - ( &
                H13(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau61(p, q) = tau61(p, q) - ( &
                tau28(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau61(p, q) = tau61(p, q) + ( &
                2 * tau51(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        tau62(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau62(p) = tau62(p) + ( &
            H11(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau62(p) = tau62(p) + ( &
            2 * H22(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau62(p) = tau62(p) + ( &
            tau3(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau62(p) = tau62(p) - ( &
            2 * tau45(p) &
        )
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
                HT22(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau63(p, q) = tau63(p, q) + ( &
                2 * t1(q) * H31(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau63(p, q) = tau63(p, q) - ( &
                2 * H40(p, q) * t1(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau63(p, q) = tau63(p, q) + ( &
                2 * tau15(p, q) &
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
                    tau63(r, p) * t3(r, p, q) &
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
            tau65(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65(p, q) = tau65(p, q) + ( &
                HT22(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65(p, q) = tau65(p, q) - ( &
                2 * H40(p, q) * t1(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau65(p, q) = tau65(p, q) + ( &
                2 * tau15(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau66(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau66(p, q) = tau66(p, q) + ( &
                    tau65(r, p) * t3(r, p, q) &
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
            tau67(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau67(p, q) = tau67(p, q) + ( &
                HT22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau67(p, q) = tau67(p, q) + ( &
                2 * t1(p) * H31(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau67(p, q) = tau67(p, q) - ( &
                2 * H40(q, p) * t1(p)**2 &
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
                    tau68(p, q, r, s) = 0.0
                end do
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
                        tau68(p, q, r, s) = tau68(p, q, r, s) + ( &
                            tau67(p, p0) * t4(p0, q, r, s) &
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
                    do p0=1, NAO
                        tau69(p, q, r, s) = tau69(p, q, r, s) + ( &
                            tau15(p0, p) * t4(p0, q, r, s) &
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
                tau70(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau70(p, q, r) = tau70(p, q, r) + ( &
                        H40(s, p) * t3(s, q, r) &
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
                    tau71(p, q, r, s) = 0.0
                end do
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
                        tau71(p, q, r, s) = tau71(p, q, r, s) + ( &
                            t3(p0, p, s) * tau70(p0, r, q) &
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
                tau72(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau72(p, q, r) = tau72(p, q, r) + ( &
                    t1(p) * t2(q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau72(p, q, r) = tau72(p, q, r) + ( &
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
                tau73(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau73(p, q, r) = tau73(p, q, r) + ( &
                    2 * t3(r, p, q)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau73(p, q, r) = tau73(p, q, r) - ( &
                    2*deltaf(q, r) * t1(q)**2 * t2(p, q)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau73(p, q, r) = tau73(p, q, r) + ( &
                    t1(p)**2 * t2(q, r)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau73(p, q, r) = tau73(p, q, r) + ( &
                    t1(q)**2 * t2(p, r)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau73(p, q, r) = tau73(p, q, r) + ( &
                    4 * t3(r, p, q) * tau72(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau73(p, q, r) = tau73(p, q, r) + ( &
                    4 * t2(p, r) * t2(q, r) * tau0(q, p) &
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
                    deltaf(p, q) * tau63(r, p) &
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
                    deltaf(q, r) * tau63(p, r) &
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
                tau75(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau75(p, q, r) = tau75(p, q, r) + ( &
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
                tau75(p, q, r) = tau75(p, q, r) + ( &
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
        do q=1, NAO
            do r=1, NAO
                tau76(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau76(p, q, r) = tau76(p, q, r) + ( &
                    2 * t3(r, p, q)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau76(p, q, r) = tau76(p, q, r) + ( &
                    t1(q)**2 * t2(p, r)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau76(p, q, r) = tau76(p, q, r) + ( &
                    4 * t2(r, p) * tau75(q, r, p) &
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
                    tau77(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau77(p, q, r, s) = tau77(p, q, r, s) + ( &
                        deltaf(p, q) * H40(r, s) &
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
                    tau77(p, q, r, s) = tau77(p, q, r, s) + ( &
                        deltaf(q, r) * H40(p, s) &
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
                tau78(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau78(p, q, r) = tau78(p, q, r) + ( &
                    t1(p) * t2(q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau78(p, q, r) = tau78(p, q, r) + ( &
                    t1(q) * t2(p, r) &
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
                tau79(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau79(p, q, r) = tau79(p, q, r) + ( &
                    2 * t3(r, p, q)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau79(p, q, r) = tau79(p, q, r) + ( &
                    t1(p)**2 * t2(q, r)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau79(p, q, r) = tau79(p, q, r) + ( &
                    t1(r)**2 * t2(p, q)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau79(p, q, r) = tau79(p, q, r) + ( &
                    4 * t3(r, p, q) * tau78(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau79(p, q, r) = tau79(p, q, r) + ( &
                    4 * t2(p, q) * t2(r, q) * tau0(r, p) &
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
                    tau80(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau80(p, q, r, s) = tau80(p, q, r, s) + ( &
                        deltaf(q, r) * H31(p, s) * t2(s, r) &
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
                    tau80(p, q, r, s) = tau80(p, q, r, s) - ( &
                        deltaf(p, q) * t2(s, p) * tau1(r, s) &
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
                    tau81(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau81(p, q, r, s) = tau81(p, q, r, s) + ( &
                        2*deltaf(p, q) * H40(s, r) * t2(r, p) &
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
                    tau81(p, q, r, s) = tau81(p, q, r, s) + ( &
                        deltaf(q, s) * t1(r) * tau1(p, r) &
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
                    tau81(p, q, r, s) = tau81(p, q, r, s) + ( &
                        deltaf(q, s) * tau38(r, p) &
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
                    tau82(p, q, r, s) = tau82(p, q, r, s) + ( &
                        deltaf(p, q) * H31(r, s) &
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
                        deltaf(p, s) * H31(r, q) &
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
                    tau83(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau83(p, q, r, s) = tau83(p, q, r, s) + ( &
                        deltaf(q, r) * H31(s, p) * t2(p, r) &
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
                    tau83(p, q, r, s) = tau83(p, q, r, s) - ( &
                        2 * t1(r) * t2(p, s) * tau77(p, q, s, r) &
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
                    tau83(p, q, r, s) = tau83(p, q, r, s) + ( &
                        t2(s, p) * tau82(q, s, r, p) &
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
                    tau83(p, q, r, s) = tau83(p, q, r, s) - ( &
                        deltaf(q, r) * t2(s, r) * tau1(p, s) &
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
                tau84(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau84(p, q, r) = tau84(p, q, r) + ( &
                    t1(r) * t2(p, q)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau84(p, q, r) = tau84(p, q, r) - ( &
                    2*deltaf(q, r) * t1(r) * t2(p, r)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau84(p, q, r) = tau84(p, q, r) + ( &
                    2 * t2(q, p) * tau50(r, p, q) &
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
                tau85(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau85(p, q, r) = tau85(p, q, r) + ( &
                    t1(r) * t2(p, q)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau85(p, q, r) = tau85(p, q, r) + ( &
                    2 * t2(p, q) * t3(r, q, p) &
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
                    tau86(p, q, r, s) = tau86(p, q, r, s) + ( &
                        deltaf(p, q) * H31(r, s) &
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
                    tau86(p, q, r, s) = tau86(p, q, r, s) + ( &
                        deltaf(q, s) * H31(r, p) &
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
            tau87(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau87(p, q) = tau87(p, q) + ( &
                H22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau87(p, q) = tau87(p, q) - ( &
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
            tau88(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau88(p, q) = tau88(p, q) + ( &
                tau87(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau88(p, q) = tau88(p, q) + ( &
                tau87(q, p) &
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
                    tau89(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau89(p, q, r, s) = tau89(p, q, r, s) + ( &
                        deltaf(p, q) * H22(r, s) &
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
                    tau89(p, q, r, s) = tau89(p, q, r, s) + ( &
                        deltaf(p, q) * H22(s, r) &
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
                    tau89(p, q, r, s) = tau89(p, q, r, s) + ( &
                        deltaf(p, q) * t1(s) * tau1(r, s) &
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
                tau90(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau90(p, q, r) = tau90(p, q, r) + ( &
                    2 * t3(r, p, q)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau90(p, q, r) = tau90(p, q, r) + ( &
                    t1(p)**2 * t2(q, r)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau90(p, q, r) = tau90(p, q, r) + ( &
                    t1(r)**2 * t2(p, q)**2 &
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
                tau91(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau91(p, q, r) = tau91(p, q, r) + ( &
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
                tau91(p, q, r) = tau91(p, q, r) + ( &
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
                tau92(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau92(p, q, r) = tau92(p, q, r) + ( &
                    t1(p) * t2(q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau92(p, q, r) = tau92(p, q, r) + ( &
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
                        deltaf(p, q) * tau91(s, p, r) &
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
                    tau93(p, q, r, s) = tau93(p, q, r, s) + ( &
                        deltaf(p, s) * tau92(r, p, q) &
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
                        deltaf(p, q) * t2(r, s) &
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
                    tau94(p, q, r, s) = tau94(p, q, r, s) + ( &
                        deltaf(q, r) * t2(p, s) &
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
                    tau95(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau95(p, q, r, s) = tau95(p, q, r, s) + ( &
                        deltaf(p, q) * t2(r, s) &
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
                    tau95(p, q, r, s) = tau95(p, q, r, s) + ( &
                        deltaf(p, s) * t2(q, r) &
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
                    tau96(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau96(p, q, r, s) = tau96(p, q, r, s) - ( &
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
                    tau96(p, q, r, s) = tau96(p, q, r, s) - ( &
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
                    tau96(p, q, r, s) = tau96(p, q, r, s) - ( &
                        t1(r) * t3(q, s, p) &
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
                    tau96(p, q, r, s) = tau96(p, q, r, s) - ( &
                        t2(q, p) * t2(r, s) &
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
                    tau96(p, q, r, s) = tau96(p, q, r, s) - ( &
                        t2(q, s) * t2(r, p) &
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
                    tau96(p, q, r, s) = tau96(p, q, r, s) + ( &
                        2 * t1(q) * tau93(q, s, r, p) &
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
                    tau96(p, q, r, s) = tau96(p, q, r, s) + ( &
                        2 * t1(r) * tau93(r, s, q, p) &
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
                    tau96(p, q, r, s) = tau96(p, q, r, s) + ( &
                        tau94(p, q, s, r) * t1(q)**2 &
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
                    tau96(p, q, r, s) = tau96(p, q, r, s) + ( &
                        tau95(r, s, q, p) * t1(r)**2 &
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
                    4*deltaf(p, q) * H31(p, r) &
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
                    4*deltaf(q, r) * H31(r, p) &
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
                    deltaf(p, q) * tau34(r) &
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
                    deltaf(q, r) * tau34(p) &
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
                tau13(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau98(p, q) = tau98(p, q) + ( &
                tau14(p, q) &
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
                    tau99(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau99(p, q, r, s) = tau99(p, q, r, s) + ( &
                        deltaf(p, s)*deltaf(q, r) &
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
                    tau99(p, q, r, s) = tau99(p, q, r, s) + ( &
                        deltaf(p, r)*deltaf(q, s) &
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
                    tau100(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau100(p, q, r, s) = tau100(p, q, r, s) + ( &
                        deltaf(p, q) * t2(r, s) &
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
                    tau100(p, q, r, s) = tau100(p, q, r, s) + ( &
                        deltaf(p, r) * t2(q, s) &
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
                tau101(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau101(p, q, r) = tau101(p, q, r) + ( &
                    H31(p, q) * t2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau101(p, q, r) = tau101(p, q, r) + ( &
                    tau70(q, r, p) &
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
                    H31(r, p) * t2(q, r) &
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
                    tau70(p, r, q) &
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
                tau103(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau103(p, q, r) = tau103(p, q, r) + ( &
                        H31(p, s) * t3(s, q, r) &
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
                tau104(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau104(p, q, r) = tau104(p, q, r) + ( &
                        H40(s, p) * t4(s, p, q, r) &
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
                tau105(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau105(p, q, r) = tau105(p, q, r) - ( &
                    tau103(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau105(p, q, r) = tau105(p, q, r) + ( &
                    2 * tau104(p, r, q) &
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
                    tau106(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau106(p, q, r, s) = tau106(p, q, r, s) - ( &
                        H31(p, q) * t3(r, q, s) &
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
                    tau106(p, q, r, s) = tau106(p, q, r, s) - ( &
                        H31(q, p) * t3(r, p, s) &
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
                    tau106(p, q, r, s) = tau106(p, q, r, s) - ( &
                        2 * H40(q, p) * tau96(s, p, q, r) &
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
                    tau106(p, q, r, s) = tau106(p, q, r, s) + ( &
                        t3(r, p, q) * tau97(q, s, p) &
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
                    tau106(p, q, r, s) = tau106(p, q, r, s) + ( &
                        t3(s, p, q) * tau97(p, r, q) &
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
                    tau106(p, q, r, s) = tau106(p, q, r, s) - ( &
                        2 * tau98(q, p) * tau99(q, p, s, r) &
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
                    tau106(p, q, r, s) = tau106(p, q, r, s) + ( &
                        tau27(p, q) * tau100(p, s, r, q) &
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
                    tau106(p, q, r, s) = tau106(p, q, r, s) + ( &
                        tau27(q, p) * tau100(q, r, s, p) &
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
                    tau106(p, q, r, s) = tau106(p, q, r, s) + ( &
                        2*deltaf(p, s) * t1(q) * tau101(p, q, r) &
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
                    tau106(p, q, r, s) = tau106(p, q, r, s) + ( &
                        2*deltaf(q, s) * t1(p) * tau101(q, p, r) &
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
                    tau106(p, q, r, s) = tau106(p, q, r, s) + ( &
                        2*deltaf(p, r) * t1(q) * tau102(q, s, p) &
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
                    tau106(p, q, r, s) = tau106(p, q, r, s) + ( &
                        2*deltaf(q, r) * t1(p) * tau102(p, s, q) &
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
                    tau106(p, q, r, s) = tau106(p, q, r, s) - ( &
                        2 * tau38(p, q) * tau47(q, r, s) &
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
                    tau106(p, q, r, s) = tau106(p, q, r, s) - ( &
                        2 * tau38(q, p) * tau47(p, s, r) &
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
                    tau106(p, q, r, s) = tau106(p, q, r, s) + ( &
                        deltaf(p, r) * tau105(q, s, p) &
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
                    tau106(p, q, r, s) = tau106(p, q, r, s) + ( &
                        deltaf(p, s) * tau105(q, r, p) &
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
                    tau106(p, q, r, s) = tau106(p, q, r, s) + ( &
                        deltaf(q, r) * tau105(p, s, q) &
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
                    tau106(p, q, r, s) = tau106(p, q, r, s) + ( &
                        deltaf(q, s) * tau105(p, r, q) &
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
                    tau107(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau107(p, q, r, s) = tau107(p, q, r, s) + ( &
                        deltaf(p, r)*deltaf(q, s) &
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
                    tau107(p, q, r, s) = tau107(p, q, r, s) + ( &
                        deltaf(p, q)*deltaf(r, s) &
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
        tau108(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau108(p) = tau108(p) + ( &
            HT22(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau108(p) = tau108(p) + ( &
            6 * t1(p) * H31(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau108(p) = tau108(p) - ( &
            4 * H22(p, p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        tau108(p) = tau108(p) + ( &
            2 * tau45(p) &
        )
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
                tau109(p, q, r) = tau109(p, q, r) + ( &
                    4 * H40(r, p) * t2(q, p) &
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
                    2*deltaf(p, q) * tau63(r, p) &
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
                    deltaf(q, r) * tau108(p) &
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
                tau110(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau110(p, q, r) = tau110(p, q, r) + ( &
                    4 * H40(r, q) * t2(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau110(p, q, r) = tau110(p, q, r) + ( &
                    2*deltaf(p, q) * tau63(r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau110(p, q, r) = tau110(p, q, r) + ( &
                    deltaf(p, r) * tau108(q) &
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
                tau111(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau111(p, q, r) = tau111(p, q, r) + ( &
                    4 * H40(q, r) * t2(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau111(p, q, r) = tau111(p, q, r) + ( &
                    deltaf(p, q) * tau108(r) &
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
                tau112(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau112(p, q, r) = tau112(p, q, r) + ( &
                    4 * H40(p, r) * t2(q, r) &
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
                    deltaf(p, q) * tau108(r) &
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
                    tau113(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau113(p, q, r, s) = tau113(p, q, r, s) - ( &
                        deltaf(p, r) * HT22(q, s) &
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
                    tau113(p, q, r, s) = tau113(p, q, r, s) - ( &
                        deltaf(q, s) * HT22(r, p) &
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
                    tau113(p, q, r, s) = tau113(p, q, r, s) - ( &
                        deltaf(r, s) * HT22(q, p) &
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
                    tau113(p, q, r, s) = tau113(p, q, r, s) - ( &
                        2*deltaf(p, r) * tau15(s, q) &
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
                    tau113(p, q, r, s) = tau113(p, q, r, s) - ( &
                        2*deltaf(q, s) * tau15(p, r) &
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
                    tau113(p, q, r, s) = tau113(p, q, r, s) - ( &
                        2*deltaf(r, s) * tau15(p, q) &
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
                    tau113(p, q, r, s) = tau113(p, q, r, s) - ( &
                        8 * tau10(s, p) * tau107(q, p, s, r) &
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
                    tau113(p, q, r, s) = tau113(p, q, r, s) + ( &
                        2*deltaf(p, q) * tau109(p, r, s) &
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
                    tau113(p, q, r, s) = tau113(p, q, r, s) + ( &
                        2*deltaf(q, s) * tau110(r, s, p) &
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
                    tau113(p, q, r, s) = tau113(p, q, r, s) + ( &
                        2*deltaf(p, r) * tau111(q, s, p) &
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
                    tau113(p, q, r, s) = tau113(p, q, r, s) + ( &
                        2*deltaf(r, s) * tau112(p, q, s) &
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
                    tau113(p, q, r, s) = tau113(p, q, r, s) - ( &
                        deltaf(p, q) * tau65(s, r) &
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
                    tau114(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau114(p, q, r, s) = tau114(p, q, r, s) - ( &
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
                    tau114(p, q, r, s) = tau114(p, q, r, s) - ( &
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
                    tau114(p, q, r, s) = tau114(p, q, r, s) - ( &
                        t1(r) * t3(q, s, p) &
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
                    tau114(p, q, r, s) = tau114(p, q, r, s) - ( &
                        t2(q, p) * t2(r, s) &
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
                    tau114(p, q, r, s) = tau114(p, q, r, s) - ( &
                        t2(q, s) * t2(r, p) &
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
                    tau114(p, q, r, s) = tau114(p, q, r, s) + ( &
                        2 * t1(r) * tau93(r, s, q, p) &
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
                    tau114(p, q, r, s) = tau114(p, q, r, s) + ( &
                        tau95(r, s, q, p) * t1(r)**2 &
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
                tau115(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau115(p, q, r) = tau115(p, q, r) + ( &
                    4*deltaf(p, q) * H31(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau115(p, q, r) = tau115(p, q, r) + ( &
                    deltaf(p, q) * tau34(r) &
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
                    tau116(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau116(p, q, r, s) = tau116(p, q, r, s) - ( &
                        H31(p, q) * t3(r, q, s) &
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
                    tau116(p, q, r, s) = tau116(p, q, r, s) - ( &
                        H31(q, p) * t3(r, p, s) &
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
                    tau116(p, q, r, s) = tau116(p, q, r, s) - ( &
                        2 * H40(q, p) * tau114(s, p, q, r) &
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
                    tau116(p, q, r, s) = tau116(p, q, r, s) + ( &
                        tau27(q, p) * tau100(q, r, s, p) &
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
                    tau116(p, q, r, s) = tau116(p, q, r, s) + ( &
                        t3(s, p, q) * tau115(q, r, p) &
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
                    tau116(p, q, r, s) = tau116(p, q, r, s) + ( &
                        t3(r, p, q) * tau115(q, s, p) &
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
                    tau116(p, q, r, s) = tau116(p, q, r, s) + ( &
                        2*deltaf(q, s) * t1(p) * tau101(q, p, r) &
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
                    tau116(p, q, r, s) = tau116(p, q, r, s) + ( &
                        2*deltaf(q, r) * t1(p) * tau102(p, s, q) &
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
                    tau116(p, q, r, s) = tau116(p, q, r, s) - ( &
                        2 * tau38(p, q) * tau47(q, r, s) &
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
                    tau116(p, q, r, s) = tau116(p, q, r, s) + ( &
                        deltaf(q, r) * tau105(p, s, q) &
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
                    tau116(p, q, r, s) = tau116(p, q, r, s) + ( &
                        deltaf(q, s) * tau105(p, r, q) &
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
                tau117(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau117(p, q, r) = tau117(p, q, r) - ( &
                    H31(p, q) * t2(r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau117(p, q, r) = tau117(p, q, r) - ( &
                    H31(q, p) * t2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau117(p, q, r) = tau117(p, q, r) + ( &
                    t2(q, p) * tau35(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau117(p, q, r) = tau117(p, q, r) + ( &
                    2 * H40(q, p) * tau26(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau117(p, q, r) = tau117(p, q, r) + ( &
                    deltaf(q, r) * t1(p) * tau27(q, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau117(p, q, r) = tau117(p, q, r) + ( &
                    deltaf(p, r) * t1(q) * tau27(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau117(p, q, r) = tau117(p, q, r) - ( &
                    deltaf(q, r) * tau30(q, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau117(p, q, r) = tau117(p, q, r) - ( &
                    deltaf(p, r) * tau30(p, q) &
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
                tau118(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau118(p, q, r) = tau118(p, q, r) - ( &
                    t2(q, p) * t2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau118(p, q, r) = tau118(p, q, r) + ( &
                    2*deltaf(q, r) * t2(p, r)**2 &
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
                    tau119(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau119(p, q, r, s) = tau119(p, q, r, s) + ( &
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
                    tau119(p, q, r, s) = tau119(p, q, r, s) + ( &
                        t2(q, p) * t2(r, s) &
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
                    tau119(p, q, r, s) = tau119(p, q, r, s) + ( &
                        t2(q, s) * t2(r, p) &
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
                    tau119(p, q, r, s) = tau119(p, q, r, s) - ( &
                        2*deltaf(q, s) * t2(p, q) * t2(r, q) &
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
                    tau119(p, q, r, s) = tau119(p, q, r, s) - ( &
                        2*deltaf(r, s) * t2(p, r) * t2(q, r) &
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
                    tau119(p, q, r, s) = tau119(p, q, r, s) + ( &
                        2*deltaf(p, r) * tau118(r, s, q) &
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
                tau120(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau120(p, q, r) = tau120(p, q, r) + ( &
                    4 * t1(p) * t3(q, p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau120(p, q, r) = tau120(p, q, r) + ( &
                    t2(q, r) * t1(p)**2 &
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
                    tau121(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau121(p, q, r, s) = tau121(p, q, r, s) + ( &
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
                    tau121(p, q, r, s) = tau121(p, q, r, s) + ( &
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
                    tau121(p, q, r, s) = tau121(p, q, r, s) + ( &
                        t1(r) * t3(q, s, p) &
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
                    tau121(p, q, r, s) = tau121(p, q, r, s) + ( &
                        t2(q, p) * t2(r, s) &
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
                    tau121(p, q, r, s) = tau121(p, q, r, s) + ( &
                        t2(q, s) * t2(r, p) &
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
                    tau121(p, q, r, s) = tau121(p, q, r, s) - ( &
                        deltaf(q, s) * t2(p, r) * t1(q)**2 &
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
                    tau121(p, q, r, s) = tau121(p, q, r, s) - ( &
                        deltaf(r, s) * t2(p, q) * t1(r)**2 &
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
                    tau121(p, q, r, s) = tau121(p, q, r, s) - ( &
                        deltaf(p, r) * tau120(r, s, q) &
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
            tau122(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau122(p, q) = tau122(p, q) + ( &
                t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau122(p, q) = tau122(p, q) - ( &
                deltaf(p, q) * t1(q)**2 &
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
                tau123(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau123(p, q, r) = tau123(p, q, r) + ( &
                    t1(p) * t2(q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau123(p, q, r) = tau123(p, q, r) + ( &
                    t1(r) * tau122(q, p) &
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
                tau124(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau124(p, q, r) = tau124(p, q, r) + ( &
                    2*deltaf(q, r) * t2(p, r)**3 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau124(p, q, r) = tau124(p, q, r) - ( &
                    tau122(q, p) * t2(r, p)**2 &
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
                tau125(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau125(p, q, r) = tau125(p, q, r) - ( &
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
                tau125(p, q, r) = tau125(p, q, r) + ( &
                    4*deltaf(q, r) * t2(p, r)**2 &
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
                    tau126(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau126(p, q, r, s) = tau126(p, q, r, s) + ( &
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
                    tau126(p, q, r, s) = tau126(p, q, r, s) + ( &
                        t1(q) * t3(r, q, s) * t1(p)**2 &
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
                    tau126(p, q, r, s) = tau126(p, q, r, s) + ( &
                        2 * t1(p) * t1(q) * tau119(s, p, q, r) &
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
                    tau126(p, q, r, s) = tau126(p, q, r, s) + ( &
                        2 * t2(q, p) * tau121(s, p, q, r) &
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
                    tau126(p, q, r, s) = tau126(p, q, r, s) + ( &
                        2 * t3(r, p, q) * tau123(q, s, p) &
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
                    tau126(p, q, r, s) = tau126(p, q, r, s) + ( &
                        4*deltaf(q, s) * tau124(q, r, p) &
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
                    tau126(p, q, r, s) = tau126(p, q, r, s) - ( &
                        t2(s, q) * tau125(p, r, q) &
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
                    tau126(p, q, r, s) = tau126(p, q, r, s) - ( &
                        t2(s, p) * tau125(q, r, p) &
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
            tau127(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau127(p, q) = tau127(p, q) + ( &
                HT22(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau127(p, q) = tau127(p, q) + ( &
                2 * t1(q) * H31(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau127(p, q) = tau127(p, q) + ( &
                2 * tau15(p, q) &
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
                tau128(p, q, r) = tau128(p, q, r) + ( &
                    tau103(q, r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau128(p, q, r) = tau128(p, q, r) - ( &
                    2 * tau104(q, r, p) &
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
                    2 * t2(r, p) * tau38(q, p) &
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
                    tau129(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau129(p, q, r, s) = tau129(p, q, r, s) - ( &
                        H31(p, r) * t3(q, r, s) &
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
                    tau129(p, q, r, s) = tau129(p, q, r, s) - ( &
                        H31(r, p) * t3(q, p, s) &
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
                    tau129(p, q, r, s) = tau129(p, q, r, s) + ( &
                        tau127(r, p) * tau95(p, s, r, q) &
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
                    tau129(p, q, r, s) = tau129(p, q, r, s) + ( &
                        t3(s, p, r) * tau115(p, q, r) &
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
                    tau129(p, q, r, s) = tau129(p, q, r, s) - ( &
                        2*deltaf(p, q)*deltaf(r, s) * tau98(p, r) &
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
                    tau129(p, q, r, s) = tau129(p, q, r, s) + ( &
                        deltaf(r, s) * t2(p, q) * tau127(p, r) &
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
                    tau129(p, q, r, s) = tau129(p, q, r, s) + ( &
                        2*deltaf(p, q) * t1(r) * tau102(r, s, p) &
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
                    tau129(p, q, r, s) = tau129(p, q, r, s) + ( &
                        2*deltaf(r, s) * t1(p) * tau101(r, p, q) &
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
                    tau129(p, q, r, s) = tau129(p, q, r, s) + ( &
                        2*deltaf(p, s) * t1(r) * tau101(p, r, q) &
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
                    tau129(p, q, r, s) = tau129(p, q, r, s) - ( &
                        2 * tau38(r, p) * tau47(p, q, s) &
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
                    tau129(p, q, r, s) = tau129(p, q, r, s) - ( &
                        deltaf(r, s) * tau128(r, p, q) &
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
                    tau129(p, q, r, s) = tau129(p, q, r, s) + ( &
                        deltaf(p, q) * tau105(r, s, p) &
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
                    tau129(p, q, r, s) = tau129(p, q, r, s) + ( &
                        deltaf(p, s) * tau105(r, q, p) &
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
                    tau130(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau130(p, q, r, s) = tau130(p, q, r, s) + ( &
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
                    tau130(p, q, r, s) = tau130(p, q, r, s) + ( &
                        t2(q, s) * t2(r, p) &
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
                    tau130(p, q, r, s) = tau130(p, q, r, s) + ( &
                        t2(p, s) * t2(q, r) &
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
                    tau130(p, q, r, s) = tau130(p, q, r, s) - ( &
                        2*deltaf(p, r) * t2(q, r) * t2(s, r) &
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
                    tau130(p, q, r, s) = tau130(p, q, r, s) - ( &
                        2*deltaf(p, s) * t2(q, s) * t2(r, s) &
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
            tau131(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau131(p, q) = tau131(p, q) + ( &
                H13(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau131(p, q) = tau131(p, q) - ( &
                H31(p, q) * t1(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau131(p, q) = tau131(p, q) + ( &
                tau28(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau131(p, q) = tau131(p, q) - ( &
                2 * tau29(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau131(p, q) = tau131(p, q) + ( &
                2 * t1(q) * tau12(q, p) &
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
                tau132(p, q, r) = tau132(p, q, r) - ( &
                    H31(p, r) * t2(q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau132(p, q, r) = tau132(p, q, r) - ( &
                    H31(r, p) * t2(q, p) &
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
                    deltaf(p, q) * t1(r) * tau63(r, p) &
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
                    deltaf(q, r) * t1(p) * tau63(p, r) &
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
                    2 * H40(r, p) * tau78(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau132(p, q, r) = tau132(p, q, r) - ( &
                    deltaf(q, r) * tau131(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau132(p, q, r) = tau132(p, q, r) - ( &
                    deltaf(p, q) * tau131(r, p) &
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
                tau133(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau133(p, q, r) = tau133(p, q, r) + ( &
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
                tau133(p, q, r) = tau133(p, q, r) + ( &
                    2 * t1(p) * tau91(r, p, q) &
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
                    tau134(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau134(p, q, r, s) = tau134(p, q, r, s) + ( &
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
                    tau134(p, q, r, s) = tau134(p, q, r, s) + ( &
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
                    tau134(p, q, r, s) = tau134(p, q, r, s) + ( &
                        t1(r) * t3(q, s, p) &
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
                    tau134(p, q, r, s) = tau134(p, q, r, s) + ( &
                        t2(q, p) * t2(r, s) &
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
                    tau134(p, q, r, s) = tau134(p, q, r, s) + ( &
                        t2(q, s) * t2(r, p) &
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
                    tau134(p, q, r, s) = tau134(p, q, r, s) - ( &
                        deltaf(r, s) * tau133(r, q, p) &
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
                tau135(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau135(p, q, r) = tau135(p, q, r) + ( &
                    tau103(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau135(p, q, r) = tau135(p, q, r) - ( &
                    2 * tau104(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau135(p, q, r) = tau135(p, q, r) + ( &
                    2 * t2(r, q) * tau38(p, r) &
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
                    tau136(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau136(p, q, r, s) = tau136(p, q, r, s) - ( &
                        H31(p, q) * t3(r, q, s) &
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
                    tau136(p, q, r, s) = tau136(p, q, r, s) - ( &
                        H31(q, p) * t3(r, p, s) &
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
                    tau136(p, q, r, s) = tau136(p, q, r, s) + ( &
                        2 * H40(q, p) * tau134(s, p, q, r) &
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
                    tau136(p, q, r, s) = tau136(p, q, r, s) + ( &
                        t3(s, p, q) * tau115(q, r, p) &
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
                    tau136(p, q, r, s) = tau136(p, q, r, s) + ( &
                        deltaf(q, r) * t2(p, s) * tau27(q, p) &
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
                    tau136(p, q, r, s) = tau136(p, q, r, s) + ( &
                        2*deltaf(q, r) * t1(p) * tau102(p, s, q) &
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
                    tau136(p, q, r, s) = tau136(p, q, r, s) - ( &
                        deltaf(q, r) * tau135(p, s, q) &
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
                tau137(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau137(p, q, r) = tau137(p, q, r) + ( &
                    2*deltaf(p, q) * tau127(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau137(p, q, r) = tau137(p, q, r) + ( &
                    deltaf(q, r) * tau108(p) &
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
                    tau138(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau138(p, q, r, s) = tau138(p, q, r, s) - ( &
                        deltaf(p, q) * HT22(r, s) &
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
                    tau138(p, q, r, s) = tau138(p, q, r, s) - ( &
                        deltaf(p, r) * HT22(q, s) &
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
                    tau138(p, q, r, s) = tau138(p, q, r, s) - ( &
                        2*deltaf(p, q) * tau15(s, r) &
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
                    tau138(p, q, r, s) = tau138(p, q, r, s) - ( &
                        2*deltaf(p, r) * tau15(s, q) &
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
                    tau138(p, q, r, s) = tau138(p, q, r, s) - ( &
                        8*deltaf(p, r)*deltaf(q, s) * tau88(p, s) &
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
                    tau138(p, q, r, s) = tau138(p, q, r, s) + ( &
                        2*deltaf(p, r) * tau137(p, q, s) &
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
                    tau138(p, q, r, s) = tau138(p, q, r, s) + ( &
                        2*deltaf(p, r)*deltaf(q, s) * tau108(s) &
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
                    tau138(p, q, r, s) = tau138(p, q, r, s) - ( &
                        deltaf(q, s) * tau65(p, r) &
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
            tau139(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau139(p, q) = tau139(p, q) + ( &
                H13(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau139(p, q) = tau139(p, q) - ( &
                t1(q) * HT22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau139(p, q) = tau139(p, q) + ( &
                tau28(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau139(p, q) = tau139(p, q) - ( &
                2 * tau59(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau139(p, q) = tau139(p, q) + ( &
                2 * t1(p) * tau38(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau139(p, q) = tau139(p, q) + ( &
                tau1(q, p) * t1(p)**2 &
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
                    tau140(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau140(p, q, r, s) = tau140(p, q, r, s) + ( &
                        deltaf(p, q) * t3(r, p, s) &
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
                    tau140(p, q, r, s) = tau140(p, q, r, s) + ( &
                        deltaf(p, s) * t3(q, p, r) &
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
                    tau141(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau141(p, q, r, s) = tau141(p, q, r, s) + ( &
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
                    tau141(p, q, r, s) = tau141(p, q, r, s) + ( &
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
                    tau142(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau142(p, q, r, s) = tau142(p, q, r, s) + ( &
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
                    tau142(p, q, r, s) = tau142(p, q, r, s) + ( &
                        t2(q, p) * t2(r, s) &
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
                    tau142(p, q, r, s) = tau142(p, q, r, s) + ( &
                        t2(q, s) * t2(r, p) &
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
                    tau143(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau143(p, q, r, s) = tau143(p, q, r, s) + ( &
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
                    tau143(p, q, r, s) = tau143(p, q, r, s) + ( &
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
                    tau144(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau144(p, q, r, s) = tau144(p, q, r, s) + ( &
                        tau141(p, q, r, s) &
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
                    tau144(p, q, r, s) = tau144(p, q, r, s) + ( &
                        tau141(q, p, r, s) &
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
                    tau144(p, q, r, s) = tau144(p, q, r, s) + ( &
                        2 * tau0(q, p) * tau142(s, p, q, r) &
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
                    tau144(p, q, r, s) = tau144(p, q, r, s) + ( &
                        2 * t3(s, p, q) * tau32(r, p, q) &
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
                    tau144(p, q, r, s) = tau144(p, q, r, s) + ( &
                        2 * t3(r, p, q) * tau72(q, s, p) &
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
                    tau144(p, q, r, s) = tau144(p, q, r, s) + ( &
                        2 * t2(q, p) * tau143(q, s, p, r) &
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
                tau145(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau145(p, q, r) = tau145(p, q, r) + ( &
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
                tau145(p, q, r) = tau145(p, q, r) + ( &
                    t1(p) * t2(q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau145(p, q, r) = tau145(p, q, r) + ( &
                    t1(q) * t2(p, r) &
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
                tau146(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau146(p, q, r) = tau146(p, q, r) - ( &
                    H31(p, r) * t2(q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau146(p, q, r) = tau146(p, q, r) - ( &
                    H31(r, p) * t2(q, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau146(p, q, r) = tau146(p, q, r) + ( &
                    deltaf(p, q) * t1(r) * tau63(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau146(p, q, r) = tau146(p, q, r) + ( &
                    2 * H40(r, p) * tau145(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau146(p, q, r) = tau146(p, q, r) - ( &
                    deltaf(p, q) * tau131(r, p) &
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
                    tau147(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau147(p, q, r, s) = tau147(p, q, r, s) + ( &
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
                    tau147(p, q, r, s) = tau147(p, q, r, s) + ( &
                        t2(q, s) * t2(r, p) &
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
                    tau147(p, q, r, s) = tau147(p, q, r, s) + ( &
                        t2(p, s) * t2(q, r) &
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
                    tau148(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau148(p, q, r, s) = tau148(p, q, r, s) + ( &
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
                    tau148(p, q, r, s) = tau148(p, q, r, s) + ( &
                        t2(q, p) * t2(r, s) &
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
                    tau148(p, q, r, s) = tau148(p, q, r, s) + ( &
                        t2(p, s) * t2(q, r) &
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
                    tau149(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau149(p, q, r, s) = tau149(p, q, r, s) + ( &
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
                    tau149(p, q, r, s) = tau149(p, q, r, s) + ( &
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
                    tau149(p, q, r, s) = tau149(p, q, r, s) - ( &
                        2*deltaf(q, r) * t1(q) * t3(p, q, s) &
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
                    tau149(p, q, r, s) = tau149(p, q, r, s) + ( &
                        t2(q, p) * t2(r, s) &
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
                    tau149(p, q, r, s) = tau149(p, q, r, s) + ( &
                        t2(p, s) * t2(q, r) &
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
                    tau150(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau150(p, q, r, s) = tau150(p, q, r, s) + ( &
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
                    tau150(p, q, r, s) = tau150(p, q, r, s) + ( &
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
                    tau150(p, q, r, s) = tau150(p, q, r, s) + ( &
                        t1(r) * t3(q, s, p) &
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
                    tau150(p, q, r, s) = tau150(p, q, r, s) + ( &
                        t2(q, p) * t2(r, s) &
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
                    tau150(p, q, r, s) = tau150(p, q, r, s) + ( &
                        t2(q, s) * t2(r, p) &
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
                    tau150(p, q, r, s) = tau150(p, q, r, s) - ( &
                        deltaf(q, s) * t2(p, r) * t1(q)**2 &
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
                    tau150(p, q, r, s) = tau150(p, q, r, s) - ( &
                        deltaf(r, s) * t2(p, q) * t1(r)**2 &
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
                    tau151(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau151(p, q, r, s) = tau151(p, q, r, s) + ( &
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
                    tau151(p, q, r, s) = tau151(p, q, r, s) + ( &
                        t1(q) * t3(r, q, s) * t1(p)**2 &
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
                    tau151(p, q, r, s) = tau151(p, q, r, s) + ( &
                        2 * t2(q, p) * tau150(s, p, q, r) &
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
                    tau151(p, q, r, s) = tau151(p, q, r, s) + ( &
                        2 * t3(r, p, q) * tau72(q, s, p) &
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
                    tau151(p, q, r, s) = tau151(p, q, r, s) - ( &
                        t2(s, q) * tau125(p, r, q) &
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
                    tau151(p, q, r, s) = tau151(p, q, r, s) - ( &
                        t2(s, p) * tau125(q, r, p) &
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
                    tau152(p, q, r, s) = tau152(p, q, r, s) + ( &
                        t1(r) * t3(q, s, p) &
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
                        t2(q, p) * t2(r, s) &
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
                        t2(q, s) * t2(r, p) &
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
                    tau153(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau153(p, q, r, s) = tau153(p, q, r, s) + ( &
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
                    tau153(p, q, r, s) = tau153(p, q, r, s) + ( &
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
                    tau153(p, q, r, s) = tau153(p, q, r, s) + ( &
                        t2(q, p) * t2(r, s) &
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
                    tau153(p, q, r, s) = tau153(p, q, r, s) + ( &
                        t2(q, s) * t2(r, p) &
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
                tau154(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau154(p, q, r) = tau154(p, q, r) + ( &
                    deltaf(p, q) * t2(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau154(p, q, r) = tau154(p, q, r) + ( &
                    deltaf(p, r) * tau122(q, p) &
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
                    tau155(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau155(p, q, r, s) = tau155(p, q, r, s) + ( &
                        tau141(p, q, r, s) &
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
                    tau155(p, q, r, s) = tau155(p, q, r, s) + ( &
                        tau141(q, p, r, s) &
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
                    tau155(p, q, r, s) = tau155(p, q, r, s) + ( &
                        2 * t3(s, p, q) * tau32(r, p, q) &
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
                    tau155(p, q, r, s) = tau155(p, q, r, s) + ( &
                        2 * t3(r, p, q) * tau72(q, s, p) &
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
                    tau155(p, q, r, s) = tau155(p, q, r, s) - ( &
                        4 * tau154(q, s, r) * t2(q, p)**2 &
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
                    tau156(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau156(p, q, r, s) = tau156(p, q, r, s) + ( &
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
                    tau156(p, q, r, s) = tau156(p, q, r, s) + ( &
                        2 * t2(q, p) * t3(r, p, s) &
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
                    tau156(p, q, r, s) = tau156(p, q, r, s) + ( &
                        2 * t2(s, p) * t3(q, p, r) &
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
                    tau156(p, q, r, s) = tau156(p, q, r, s) + ( &
                        2 * t1(p) * tau152(s, p, q, r) &
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
                    tau156(p, q, r, s) = tau156(p, q, r, s) + ( &
                        2 * t2(r, p) * tau39(s, p, q) &
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
                    tau157(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau157(p, q, r, s) = tau157(p, q, r, s) + ( &
                        tau141(p, q, r, s) &
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
                    tau157(p, q, r, s) = tau157(p, q, r, s) + ( &
                        tau141(q, p, r, s) &
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
                    tau157(p, q, r, s) = tau157(p, q, r, s) + ( &
                        2 * t1(p) * t1(q) * tau142(s, p, q, r) &
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
                    tau157(p, q, r, s) = tau157(p, q, r, s) + ( &
                        8 * tau99(q, p, s, r) * t2(q, p)**3 &
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
            tau158(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau158(p, q) = tau158(p, q) - ( &
                H13(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau158(p, q) = tau158(p, q) + ( &
                H31(p, q) * t1(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau158(p, q) = tau158(p, q) - ( &
                tau28(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau158(p, q) = tau158(p, q) + ( &
                2 * tau29(p, q) &
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
                tau159(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau159(p, q, r) = tau159(p, q, r) - ( &
                    H31(p, r) * t2(q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau159(p, q, r) = tau159(p, q, r) - ( &
                    H31(r, p) * t2(q, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau159(p, q, r) = tau159(p, q, r) + ( &
                    deltaf(p, q) * t1(r) * tau23(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau159(p, q, r) = tau159(p, q, r) + ( &
                    deltaf(p, q) * tau158(r, p) &
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
                    tau160(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau160(p, q, r, s) = tau160(p, q, r, s) + ( &
                        t2(q, s) * t2(r, s) * t1(p)**2 &
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
                    tau160(p, q, r, s) = tau160(p, q, r, s) + ( &
                        t1(p) * t3(q, p, r) * t1(s)**2 &
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
                    tau160(p, q, r, s) = tau160(p, q, r, s) + ( &
                        t1(s) * t3(q, s, r) * t1(p)**2 &
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
                    tau160(p, q, r, s) = tau160(p, q, r, s) + ( &
                        2 * t3(s, p, q) * tau78(p, s, r) &
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
                    tau160(p, q, r, s) = tau160(p, q, r, s) - ( &
                        t2(r, p) * tau125(s, q, p) &
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
                    tau161(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau161(p, q, r, s) = tau161(p, q, r, s) + ( &
                        deltaf(p, q) * HT22(r, s) &
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
                    tau161(p, q, r, s) = tau161(p, q, r, s) + ( &
                        deltaf(p, r) * HT22(q, s) &
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
                    tau161(p, q, r, s) = tau161(p, q, r, s) + ( &
                        2*deltaf(p, q) * tau15(s, r) &
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
                    tau161(p, q, r, s) = tau161(p, q, r, s) + ( &
                        2*deltaf(p, r) * tau15(s, q) &
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
                    tau161(p, q, r, s) = tau161(p, q, r, s) - ( &
                        4*deltaf(p, q)*deltaf(p, r) * tau27(p, s) &
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
                    tau162(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau162(p, q, r, s) = tau162(p, q, r, s) + ( &
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
                    tau162(p, q, r, s) = tau162(p, q, r, s) + ( &
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
                tau163(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau163(p, q, r) = tau163(p, q, r) + ( &
                        H40(s, q) * tau162(s, p, q, r) &
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
                tau164(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau164(p, q, r) = tau164(p, q, r) - ( &
                    tau103(q, r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau164(p, q, r) = tau164(p, q, r) + ( &
                    2 * tau163(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau164(p, q, r) = tau164(p, q, r) + ( &
                    t2(r, q) * tau27(p, q) &
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
                    tau165(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau165(p, q, r, s) = tau165(p, q, r, s) + ( &
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
                    tau165(p, q, r, s) = tau165(p, q, r, s) + ( &
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
                    tau165(p, q, r, s) = tau165(p, q, r, s) + ( &
                        t2(q, p) * t2(r, s) &
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
                    tau165(p, q, r, s) = tau165(p, q, r, s) + ( &
                        t2(p, s) * t2(q, r) &
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
                    tau166(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau166(p, q, r, s) = tau166(p, q, r, s) + ( &
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
                    tau166(p, q, r, s) = tau166(p, q, r, s) + ( &
                        2 * t1(r) * t2(q, p) * t2(s, p) &
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
                    tau166(p, q, r, s) = tau166(p, q, r, s) + ( &
                        2 * t1(p) * tau165(s, p, q, r) &
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
                    tau167(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau167(p, q, r, s) = tau167(p, q, r, s) + ( &
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
                    tau167(p, q, r, s) = tau167(p, q, r, s) + ( &
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
                    tau167(p, q, r, s) = tau167(p, q, r, s) + ( &
                        t2(q, s) * t2(r, p) &
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
                    tau167(p, q, r, s) = tau167(p, q, r, s) + ( &
                        t2(p, s) * t2(q, r) &
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
                    tau168(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau168(p, q, r, s) = tau168(p, q, r, s) + ( &
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
                    tau168(p, q, r, s) = tau168(p, q, r, s) + ( &
                        2 * t1(s) * t2(q, p) * t2(r, p) &
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
                    tau168(p, q, r, s) = tau168(p, q, r, s) + ( &
                        2 * t1(p) * tau167(s, p, q, r) &
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
                    tau169(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau169(p, q, r, s) = tau169(p, q, r, s) + ( &
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
                    tau169(p, q, r, s) = tau169(p, q, r, s) + ( &
                        2 * t2(q, p) * t3(r, p, s) &
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
                    tau169(p, q, r, s) = tau169(p, q, r, s) + ( &
                        2 * t2(s, p) * t3(q, p, r) &
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
                    tau169(p, q, r, s) = tau169(p, q, r, s) + ( &
                        2 * t2(r, p) * tau39(s, p, q) &
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
                    tau170(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau170(p, q, r, s) = tau170(p, q, r, s) + ( &
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
                    tau170(p, q, r, s) = tau170(p, q, r, s) + ( &
                        2 * t2(s, p) * t3(q, p, r) &
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
                    tau170(p, q, r, s) = tau170(p, q, r, s) + ( &
                        2 * t1(p) * t1(r) * t3(q, p, s) &
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
                    tau170(p, q, r, s) = tau170(p, q, r, s) + ( &
                        2 * t2(q, p) * tau39(s, p, r) &
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
                    tau171(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau171(p, q, r, s) = tau171(p, q, r, s) + ( &
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
                    tau171(p, q, r, s) = tau171(p, q, r, s) + ( &
                        2 * t2(s, p) * t3(q, p, r) &
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
                    tau171(p, q, r, s) = tau171(p, q, r, s) + ( &
                        2 * t1(p) * t1(q) * t3(r, p, s) &
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
                    tau171(p, q, r, s) = tau171(p, q, r, s) + ( &
                        2 * t2(r, p) * tau39(s, p, q) &
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
                    tau172(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau172(p, q, r, s) = tau172(p, q, r, s) + ( &
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
                    tau172(p, q, r, s) = tau172(p, q, r, s) + ( &
                        2 * tau0(s, p) * t3(r, p, q) &
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
                    tau172(p, q, r, s) = tau172(p, q, r, s) + ( &
                        2 * t2(r, p) * tau40(s, p, q) &
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
                    tau173(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau173(p, q, r, s) = tau173(p, q, r, s) + ( &
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
                    tau173(p, q, r, s) = tau173(p, q, r, s) + ( &
                        t1(s) * t3(q, r, p) &
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
                    tau174(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau174(p, q, r, s) = tau174(p, q, r, s) + ( &
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
                    tau174(p, q, r, s) = tau174(p, q, r, s) + ( &
                        t1(q) * t3(r, s, p) &
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
                    tau175(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau175(p, q, r, s) = tau175(p, q, r, s) + ( &
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
                    tau175(p, q, r, s) = tau175(p, q, r, s) + ( &
                        2 * t1(p) * t1(s) * t3(q, p, r) &
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
                    tau175(p, q, r, s) = tau175(p, q, r, s) + ( &
                        2 * t2(q, p) * tau40(s, p, r) &
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
                    tau176(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau176(p, q, r, s) = tau176(p, q, r, s) + ( &
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
                    tau176(p, q, r, s) = tau176(p, q, r, s) + ( &
                        2 * t1(p) * t1(r) * t3(q, p, s) &
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
                    tau176(p, q, r, s) = tau176(p, q, r, s) + ( &
                        2 * t2(q, p) * tau39(s, p, r) &
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
                    tau177(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau177(p, q, r, s) = tau177(p, q, r, s) + ( &
                        deltaf(p, q) * HT22(r, s) &
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
                    tau177(p, q, r, s) = tau177(p, q, r, s) + ( &
                        deltaf(q, s) * HT22(r, p) &
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
                    tau177(p, q, r, s) = tau177(p, q, r, s) + ( &
                        2*deltaf(p, q) * tau15(s, r) &
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
                    tau177(p, q, r, s) = tau177(p, q, r, s) + ( &
                        2*deltaf(q, s) * tau15(p, r) &
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
                    tau178(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau178(p, q, r, s) = tau178(p, q, r, s) + ( &
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
                    tau178(p, q, r, s) = tau178(p, q, r, s) + ( &
                        2 * t1(r) * t2(q, p) * t2(s, p) &
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
            tau179(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau179(p, q) = tau179(p, q) + ( &
                    t3(r, p, q) * tau70(r, q, p) &
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
                tau180(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau180(p, q, r) = tau180(p, q, r) + ( &
                        H31(p, s) * t4(s, p, q, r) &
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
                    tau181(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau181(p, q, r, s) = tau181(p, q, r, s) + ( &
                        t2(q, p) * tau70(p, r, s) &
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
                    tau181(p, q, r, s) = tau181(p, q, r, s) + ( &
                        deltaf(p, q) * tau180(p, r, s) &
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
                    tau182(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau182(p, q, r, s) = tau182(p, q, r, s) + ( &
                        deltaf(p, q) * t3(r, p, s) &
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
                    tau182(p, q, r, s) = tau182(p, q, r, s) + ( &
                        deltaf(p, r) * t3(q, p, s) &
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
                    tau182(p, q, r, s) = tau182(p, q, r, s) + ( &
                        deltaf(p, s) * t3(q, p, r) &
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
            tau183(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau183(p, q) = tau183(p, q) + ( &
                3 * H31(p, p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau183(p, q) = tau183(p, q) + ( &
                tau29(p, q) &
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
                tau184(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau184(p, q, r) = tau184(p, q, r) - ( &
                    tau70(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau184(p, q, r) = tau184(p, q, r) + ( &
                    2*deltaf(p, r) * tau183(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau184(p, q, r) = tau184(p, q, r) + ( &
                    2*deltaf(p, q) * tau183(p, r) &
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
                tau185(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau185(p, q, r) = tau185(p, q, r) - ( &
                    tau180(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau185(p, q, r) = tau185(p, q, r) + ( &
                    2 * t2(q, p) * tau29(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau185(p, q, r) = tau185(p, q, r) + ( &
                    2 * t2(r, p) * tau183(p, q) &
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
                    tau186(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau186(p, q, r, s) = tau186(p, q, r, s) + ( &
                        2 * tau181(p, r, q, s) &
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
                    tau186(p, q, r, s) = tau186(p, q, r, s) + ( &
                        2 * tau181(p, s, q, r) &
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
                    tau186(p, q, r, s) = tau186(p, q, r, s) - ( &
                        2 * tau46(p) * tau182(p, s, r, q) &
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
                    tau186(p, q, r, s) = tau186(p, q, r, s) - ( &
                        2 * t2(q, p) * tau184(p, s, r) &
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
                    tau186(p, q, r, s) = tau186(p, q, r, s) - ( &
                        2*deltaf(p, q) * tau185(p, s, r) &
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
                    tau186(p, q, r, s) = tau186(p, q, r, s) + ( &
                        tau34(p) * t4(s, p, q, r) &
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
                    tau186(p, q, r, s) = tau186(p, q, r, s) + ( &
                        tau48(q, p) * t3(s, p, r) &
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
                    tau186(p, q, r, s) = tau186(p, q, r, s) + ( &
                        tau23(p, r) * t3(s, p, q) &
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
                    tau186(p, q, r, s) = tau186(p, q, r, s) + ( &
                        tau48(s, p) * t3(r, p, q) &
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
                    tau186(p, q, r, s) = tau186(p, q, r, s) - ( &
                        4 * tau29(p, q) * tau47(p, s, r) &
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
                tau187(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau187(p, q, r) = tau187(p, q, r) - ( &
                    tau180(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau187(p, q, r) = tau187(p, q, r) + ( &
                    2 * t2(q, p) * tau29(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau187(p, q, r) = tau187(p, q, r) + ( &
                    tau46(p) * t3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau187(p, q, r) = tau187(p, q, r) + ( &
                    2 * t2(r, p) * tau183(p, q) &
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
                    tau188(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau188(p, q, r, s) = tau188(p, q, r, s) + ( &
                        2 * t2(q, p) * tau70(p, r, s) &
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
                    tau188(p, q, r, s) = tau188(p, q, r, s) + ( &
                        2 * t2(r, p) * tau70(p, q, s) &
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
                    tau188(p, q, r, s) = tau188(p, q, r, s) + ( &
                        2 * t2(s, p) * tau70(p, q, r) &
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
                    tau188(p, q, r, s) = tau188(p, q, r, s) - ( &
                        2*deltaf(p, q) * tau187(p, s, r) &
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
                    tau188(p, q, r, s) = tau188(p, q, r, s) - ( &
                        2*deltaf(p, s) * tau187(p, r, q) &
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
                    tau188(p, q, r, s) = tau188(p, q, r, s) + ( &
                        tau34(p) * t4(s, p, q, r) &
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
                    tau188(p, q, r, s) = tau188(p, q, r, s) + ( &
                        tau48(q, p) * t3(s, p, r) &
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
                    tau188(p, q, r, s) = tau188(p, q, r, s) + ( &
                        tau23(p, r) * t3(s, p, q) &
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
                    tau188(p, q, r, s) = tau188(p, q, r, s) + ( &
                        tau48(s, p) * t3(r, p, q) &
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
                tau189(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau189(p, q, r) = tau189(p, q, r) + ( &
                        t3(s, p, q) * tau70(s, r, p) &
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
            tau190(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau190(p, q) = tau190(p, q) + ( &
                tau13(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau190(p, q) = tau190(p, q) + ( &
                tau108(p) * t2(q, p) &
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
                tau191(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau191(p, q, r) = tau191(p, q, r) - ( &
                    2 * tau189(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau191(p, q, r) = tau191(p, q, r) + ( &
                    2*deltaf(q, r) * tau179(p, r) &
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
                    tau191(p, q, r) = tau191(p, q, r) - ( &
                        tau24(p, s) * t4(s, p, q, r) &
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
                tau191(p, q, r) = tau191(p, q, r) + ( &
                    2 * tau52(p) * t3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau191(p, q, r) = tau191(p, q, r) + ( &
                    2 * t2(r, p) * tau190(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau191(p, q, r) = tau191(p, q, r) + ( &
                    2 * t2(q, p) * tau13(p, r) &
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
                tau192(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau192(p, q, r) = tau192(p, q, r) - ( &
                    tau103(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau192(p, q, r) = tau192(p, q, r) + ( &
                    2 * tau104(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau192(p, q, r) = tau192(p, q, r) + ( &
                    2*deltaf(p, q) * tau44(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau192(p, q, r) = tau192(p, q, r) + ( &
                    2*deltaf(p, r) * tau44(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau192(p, q, r) = tau192(p, q, r) - ( &
                    2 * tau46(p) * tau47(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau192(p, q, r) = tau192(p, q, r) + ( &
                    tau34(p) * t3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau192(p, q, r) = tau192(p, q, r) + ( &
                    t2(r, p) * tau48(q, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau192(p, q, r) = tau192(p, q, r) + ( &
                    t2(q, p) * tau23(p, r) &
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
            tau193(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau193(p, q) = tau193(p, q) + ( &
                HT22(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau193(p, q) = tau193(p, q) + ( &
                2 * tau15(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau193(p, q) = tau193(p, q) - ( &
                2*deltaf(p, q) * tau46(p) &
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
                tau194(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau194(p, q, r) = tau194(p, q, r) - ( &
                    tau103(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau194(p, q, r) = tau194(p, q, r) + ( &
                    2 * tau104(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau194(p, q, r) = tau194(p, q, r) + ( &
                    2*deltaf(p, q) * tau44(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau194(p, q, r) = tau194(p, q, r) + ( &
                    2*deltaf(p, r) * tau44(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau194(p, q, r) = tau194(p, q, r) + ( &
                    t2(q, p) * tau193(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau194(p, q, r) = tau194(p, q, r) + ( &
                    tau34(p) * t3(r, p, q) &
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
                    tau195(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau195(p, q, r, s) = tau195(p, q, r, s) + ( &
                        t1(p) * t4(r, p, s, q) &
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
                    tau195(p, q, r, s) = tau195(p, q, r, s) + ( &
                        t2(q, p) * t3(r, p, s) &
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
                    tau195(p, q, r, s) = tau195(p, q, r, s) + ( &
                        t2(r, p) * t3(q, p, s) &
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
                    tau195(p, q, r, s) = tau195(p, q, r, s) + ( &
                        t2(s, p) * t3(q, p, r) &
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
            tau196(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau196(p, q) = tau196(p, q) - ( &
                H13(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau196(p, q) = tau196(p, q) - ( &
                tau28(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau196(p, q) = tau196(p, q) + ( &
                2 * tau29(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau196(p, q) = tau196(p, q) + ( &
                2*deltaf(p, q) * tau55(p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau196(p, q) = tau196(p, q) + ( &
                tau34(p) * t2(q, p) &
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
                    tau197(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau197(p, q, r, s) = tau197(p, q, r, s) + ( &
                        2 * t2(p, q) * tau70(q, r, s) &
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
                    tau197(p, q, r, s) = tau197(p, q, r, s) + ( &
                        2 * t2(r, q) * tau70(q, p, s) &
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
                    tau197(p, q, r, s) = tau197(p, q, r, s) + ( &
                        2 * t2(s, q) * tau70(q, p, r) &
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
                    tau197(p, q, r, s) = tau197(p, q, r, s) + ( &
                        tau48(p, q) * t3(s, q, r) &
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
                    tau197(p, q, r, s) = tau197(p, q, r, s) + ( &
                        tau23(q, r) * t3(s, p, q) &
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
                    tau197(p, q, r, s) = tau197(p, q, r, s) + ( &
                        tau48(s, q) * t3(r, p, q) &
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
            tau198(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau198(p, q) = tau198(p, q) + ( &
                HT22(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau198(p, q) = tau198(p, q) - ( &
                2 * H40(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau198(p, q) = tau198(p, q) + ( &
                2 * tau15(q, p) &
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
                tau199(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    tau199(p, q, r) = tau199(p, q, r) + ( &
                        tau198(p, s) * t4(s, p, q, r) &
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
                tau200(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau200(p, q, r) = tau200(p, q, r) + ( &
                    2 * t2(r, p) * tau44(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau200(p, q, r) = tau200(p, q, r) + ( &
                    2 * tau189(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau200(p, q, r) = tau200(p, q, r) + ( &
                    tau199(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau200(p, q, r) = tau200(p, q, r) + ( &
                    2 * tau55(p) * t3(r, p, q) &
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
                tau201(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau201(p, q, r) = tau201(p, q, r) + ( &
                    2 * t2(q, p) * tau44(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau201(p, q, r) = tau201(p, q, r) + ( &
                    2 * tau189(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau201(p, q, r) = tau201(p, q, r) + ( &
                    tau199(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau201(p, q, r) = tau201(p, q, r) + ( &
                    2 * tau55(p) * t3(r, p, q) &
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
                tau202(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau202(p, q, r) = tau202(p, q, r) - ( &
                    tau103(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau202(p, q, r) = tau202(p, q, r) + ( &
                    2 * tau104(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau202(p, q, r) = tau202(p, q, r) + ( &
                    t2(q, p) * tau23(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau202(p, q, r) = tau202(p, q, r) + ( &
                    t2(r, p) * tau48(q, p) &
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
                tau203(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau203(p, q, r) = tau203(p, q, r) - ( &
                    tau103(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau203(p, q, r) = tau203(p, q, r) + ( &
                    2 * tau104(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau203(p, q, r) = tau203(p, q, r) + ( &
                    2*deltaf(p, q) * tau44(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau203(p, q, r) = tau203(p, q, r) + ( &
                    t2(q, p) * tau23(p, r) &
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
                tau204(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau204(p, q, r) = tau204(p, q, r) + ( &
                    2 * t2(q, p) * tau44(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau204(p, q, r) = tau204(p, q, r) + ( &
                    2 * tau189(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau204(p, q, r) = tau204(p, q, r) + ( &
                    tau199(p, q, r) &
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
                tau205(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau205(p, q, r) = tau205(p, q, r) - ( &
                    tau103(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau205(p, q, r) = tau205(p, q, r) + ( &
                    2 * tau104(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau205(p, q, r) = tau205(p, q, r) + ( &
                    t2(q, p) * tau23(p, r) &
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
                tau206(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau206(p, q, r) = tau206(p, q, r) + ( &
                    2 * tau189(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                tau206(p, q, r) = tau206(p, q, r) + ( &
                    tau199(p, q, r) &
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
            tau207(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau207(p, q) = tau207(p, q) + ( &
                H13(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau207(p, q) = tau207(p, q) + ( &
                tau28(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau207(p, q) = tau207(p, q) - ( &
                2 * tau29(q, p) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau208(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau208(p, q) = tau208(p, q) + ( &
                H13(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau208(p, q) = tau208(p, q) + ( &
                tau28(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau208(p, q) = tau208(p, q) - ( &
                2 * tau29(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp single
    Ene = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:)
    do p=1, NAO
        Ene = Ene + ( &
            H20(p) * t1(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static) reduction(+:)
    do p=1, NAO
        do q=1, NAO
            Ene = Ene + ( &
                H40(p, q) * tau0(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        Res1(p) = 0.0
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        Res1(p) = Res1(p) - ( &
            H20(p) * t1(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        Res1(p) = Res1(p) - ( &
            2 * H31(p, p) * t1(p)**2 &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        Res1(p) = Res1(p) + ( &
            H02(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res1(p) = Res1(p) + ( &
                    H40(r, q) * t3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        Res1(p) = Res1(p) - ( &
            2 * tau2(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        Res1(p) = Res1(p) + ( &
            2 * t1(p) * tau4(p) &
        )
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) + ( &
                tau6(q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res1(p) = Res1(p) - ( &
                t1(q) * tau7(p, q) &
            )
        end do
    end do
    !$omp end do


    !$omp end parallel



    !$omp parallel default(shared)

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res2(p, q) = 0.0
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res2(p, q) = Res2(p, q) + ( &
                2 * H04(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res2(p, q) = Res2(p, q) - ( &
                HT22(p, q) * t1(q)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res2(p, q) = Res2(p, q) + ( &
                2 * t1(q) * H13(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    HT22(q, r) * t2(r, p) &
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
                    Res2(p, q) = Res2(p, q) + ( &
                        H40(s, r) * t4(s, q, p, r) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res2(p, q) = Res2(p, q) + ( &
                2 * tau9(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res2(p, q) = Res2(p, q) + ( &
                2 * tau9(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res2(p, q) = Res2(p, q) + ( &
                4 * t2(q, p) * tau10(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res2(p, q) = Res2(p, q) + ( &
                2 * H40(q, p) * tau11(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res2(p, q) = Res2(p, q) + ( &
                4 * t1(p) * t1(q) * tau12(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res2(p, q) = Res2(p, q) - ( &
                2 * tau13(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res2(p, q) = Res2(p, q) - ( &
                2 * tau14(p, q) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res2(p, q) = Res2(p, q) - ( &
                2*deltaf(p, q) * tau18(p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res2(p, q) = Res2(p, q) - ( &
                2 * tau20(p) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res2(p, q) = Res2(p, q) - ( &
                2 * tau20(q) * t2(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    tau6(r) * t3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res2(p, q) = Res2(p, q) + ( &
                2 * t1(p) * tau21(q, p) &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            Res2(p, q) = Res2(p, q) - ( &
                tau22(q, p) * t1(p)**2 &
            )
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res2(p, q) = Res2(p, q) + ( &
                    t2(r, q) * tau23(r, p) &
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
                Res3(p, q, r) = 0.0
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    tau25(q, r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    tau25(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    tau25(p, q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    4 * t2(q, p) * tau31(q, p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    4 * t2(r, p) * tau33(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau35(p, r, q) * t2(q, p)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    4 * tau36(r, q) * tau37(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    4 * tau38(q, p) * tau39(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    4 * tau38(r, p) * tau40(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau1(p, r) * tau41(r, q, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau1(p, q) * tau42(q, r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau1(q, r) * tau42(r, q, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    4 * tau0(r, q) * tau43(q, r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    4 * t1(p) * t2(q, r) * tau38(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    4 * t1(p) * t2(q, r) * tau38(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2*deltaf(p, q) * tau34(r) * t2(r, p)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * t2(q, r) * tau1(q, p) * t1(p)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * t2(q, r) * tau1(r, p) * t1(p)**2 &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * H31(r, q) * tau42(q, r, p) &
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
                    Res3(p, q, r) = Res3(p, q, r) - ( &
                        2 * tau1(p, s) * t4(s, p, q, r) &
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
                    Res3(p, q, r) = Res3(p, q, r) - ( &
                        2 * tau1(q, s) * t4(s, p, q, r) &
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
                    Res3(p, q, r) = Res3(p, q, r) - ( &
                        2 * tau1(r, s) * t4(s, p, q, r) &
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
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t1(p) * tau49(p, r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(q, p) * tau53(q, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(r, p) * tau56(q, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(r, p) * tau57(r, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau20(q) * t3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * tau20(r) * t3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(q, p) * tau58(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(r, q) * tau60(p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2 * t2(r, q) * tau61(r, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) + ( &
                    2 * tau62(p) * t3(r, p, q) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2*deltaf(q, r) * tau64(q, p) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2*deltaf(p, q) * tau66(p, r) &
                )
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                Res3(p, q, r) = Res3(p, q, r) - ( &
                    2*deltaf(p, r) * tau66(p, q) &
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
                    Res3(p, q, r) = Res3(p, q, r) + ( &
                        tau6(s) * t4(s, p, q, r) &
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
                    Res4(p, q, r, s) = 0.0
                end do
            end do
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            do r=1, NAO
                do s=1, NAO
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        tau68(p, r, s, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        tau68(q, r, s, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        tau68(r, q, s, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        tau68(s, q, r, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau69(p, s, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau69(q, p, r, s) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau69(r, p, q, s) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau69(s, p, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau71(p, q, r, s) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau71(p, q, s, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau71(p, s, r, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        4*deltaf(q, r) * H40(p, s) * tau73(s, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        4*deltaf(r, s) * tau74(p, r, q) * t2(p, q)**2 &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        4 * tau76(r, p, q) * tau77(r, s, q, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        4*deltaf(p, r) * H40(q, s) * tau79(s, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        4*deltaf(p, q) * H40(r, s) * tau79(s, p, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        8 * tau39(r, p, q) * tau80(p, s, r, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        8 * t2(p, q) * t2(r, q) * tau81(p, s, r, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        8 * t3(r, p, q) * tau83(r, s, q, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4*deltaf(q, r) * H31(p, s) * tau84(s, q, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        4*deltaf(p, q)*deltaf(q, s) * tau63(r, p) * t2(p, r)**2 &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * tau85(q, p, s) * tau86(q, r, s, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        8*deltaf(p, r) * t2(q, p) * t2(s, p) * tau88(q, s) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        8*deltaf(p, q) * t2(r, p) * t2(s, p) * tau88(s, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        8 * t2(p, r) * t2(q, r) * tau89(r, s, q, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        4*deltaf(p, s) * H40(q, r) * tau90(r, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        8*deltaf(p, s) * t2(q, p) * t2(r, p) * tau38(q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        8*deltaf(q, r) * t2(p, q) * t2(s, q) * tau38(s, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4*deltaf(p, r) * H31(q, s) * tau85(s, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4*deltaf(p, q) * H31(s, r) * tau85(r, p, s) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4*deltaf(p, q) * H31(r, s) * tau85(s, p, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        4*deltaf(p, q)*deltaf(q, r) * tau23(s, p) * t2(p, s)**2 &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * t1(q) * tau86(r, s, q, p) * t2(p, r)**2 &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * t1(r) * tau86(p, s, r, q) * t2(p, q)**2 &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * t1(p) * tau86(q, s, p, r) * t2(q, r)**2 &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * t2(q, p) * tau106(q, p, s, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * tau113(q, s, r, p) * t2(q, p)**2 &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * t2(s, p) * tau116(s, p, r, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * t3(r, p, q) * tau117(r, p, s) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * t3(r, p, q) * tau117(r, q, s) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * H40(r, p) * tau126(r, p, s, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * t2(r, p) * tau129(p, q, r, s) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * t3(s, p, q) * tau31(q, p, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * tau10(r, q) * tau130(s, p, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * t3(r, p, q) * tau132(q, s, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * t2(s, q) * tau136(s, q, r, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * tau138(p, s, q, r) * t2(r, p)**2 &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        4 * tau139(p, s) * tau140(p, q, s, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * H40(s, r) * tau144(s, r, q, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * t3(s, p, q) * tau146(q, r, s) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * tau10(s, p) * tau147(s, p, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * tau10(s, q) * tau148(s, p, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * tau38(p, r) * tau149(s, p, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * H40(r, q) * tau151(r, q, s, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * tau38(q, p) * tau152(s, p, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * tau38(s, r) * tau153(s, p, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * H40(s, p) * tau155(s, p, r, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * H31(r, s) * tau156(s, r, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * H40(q, p) * tau157(q, p, s, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * t3(s, p, r) * tau159(p, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * H40(s, q) * tau160(q, r, p, s) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau161(p, r, q, s) * t2(s, p)**2 &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4*deltaf(r, s) * t2(q, r) * tau164(r, q, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4*deltaf(q, s) * t2(r, q) * tau164(q, r, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * H31(p, q) * tau166(q, s, p, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * H31(r, p) * tau168(p, s, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * H31(s, r) * tau169(r, s, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * H31(p, s) * tau170(s, r, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * H31(s, p) * tau171(p, s, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * H31(q, r) * tau172(r, s, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * H31(r, q) * tau172(q, s, p, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * tau12(s, p) * tau143(s, r, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * tau12(q, r) * tau173(q, s, p, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * tau12(q, s) * tau174(q, s, p, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * H31(q, s) * tau175(s, p, r, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * H31(s, q) * tau176(q, p, s, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau177(q, s, p, r) * t2(r, q)**2 &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * t1(p) * tau12(q, p) * t3(r, s, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * t1(r) * tau12(p, r) * t3(q, s, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * t1(r) * tau12(r, s) * t3(q, s, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * H31(q, p) * tau178(p, s, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * H31(p, r) * tau178(r, s, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2*deltaf(q, r) * tau23(s, p) * t2(s, q)**2 &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        4 * tau179(q, p) * tau99(q, p, s, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * t1(p) * tau186(p, s, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * t1(q) * tau188(q, s, p, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2*deltaf(r, s) * tau191(r, q, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * t2(q, p) * tau192(q, s, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * t2(q, p) * tau192(p, s, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * t2(s, p) * tau194(p, r, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * tau34(r) * tau195(r, s, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * tau34(s) * tau195(s, r, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * tau196(p, r) * t3(s, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * tau196(q, r) * t3(s, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * tau56(s, q) * t3(r, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * t1(r) * tau197(s, r, q, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * t1(s) * tau197(p, s, r, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2*deltaf(p, q) * tau200(p, s, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2*deltaf(p, s) * tau201(p, r, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * t2(r, p) * tau202(r, s, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * t2(s, p) * tau202(s, r, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * t2(r, q) * tau203(q, s, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2*deltaf(q, r) * tau204(q, s, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau62(p) * t4(s, p, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau62(q) * t4(s, p, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau62(r) * t4(s, p, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau62(s) * t4(s, p, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * t2(r, q) * tau205(r, s, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * t2(s, q) * tau205(s, r, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2*deltaf(p, r) * tau206(p, s, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2*deltaf(q, s) * tau206(q, r, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau207(p, q) * t3(s, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau208(r, p) * t3(s, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau207(p, s) * t3(s, q, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau207(q, p) * t3(s, p, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau208(r, q) * t3(s, p, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau207(q, s) * t3(s, p, r) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau208(s, r) * t3(s, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau207(s, p) * t3(r, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) + ( &
                        2 * tau208(r, s) * t3(r, p, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * t2(r, p) * tau105(p, s, q) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * t2(s, r) * tau105(s, q, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * t2(s, q) * tau105(q, r, p) &
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
                    Res4(p, q, r, s) = Res4(p, q, r, s) - ( &
                        2 * t2(s, r) * tau105(r, q, p) &
                    )
                end do
            end do
        end do
    end do
    !$omp end do


    !$omp end parallel


    End Subroutine 
  pure double precision function deltaf(p, q)
  implicit none
  integer, intent(in) :: p, q

  if (p == q) then
    deltaf = 1.0d0
  else
    deltaf = 0.0d0
  end if

end function deltaf

End Module CCResCCSDTQ
