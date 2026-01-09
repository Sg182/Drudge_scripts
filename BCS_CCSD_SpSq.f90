    Module CCSpSq
    Use Precision
    Use Constants

    Contains

Subroutine CCSD_SpSq(SzSz,z1,z2,T1,T2,NAO,H20,H11,H02,H40,H31,H22,HT22,H13,H04)
    Implicit None
    Integer,           Intent(In)    :: NAO
    Complex (Kind=pr), Intent(In)    :: T1(NAO), T2(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: z1(NAO), z2(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H20(NAO), H11(NAO), H02(NAO)
    Complex (Kind=pr), Intent(In)    :: H40(NAO,NAO), H31(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H04(NAO,NAO), H13(NAO,NAO)
    Complex (Kind=pr), Intent(In)    :: H22(NAO,NAO), HT22(NAO,NAO)
    Complex (Kind=pr), Intent(Out)   :: SpSq(NAO,NAO) 
    Integer                          :: p, q, r, s, i, j, k, l

    complex(kind=pr) :: one(NAO)
    one = 1.0_pr
    complex(kind=pr) :: tau0

    complex(kind=pr) , dimension(:), allocatable :: tau1

    complex(kind=pr) :: tau2

    complex(kind=pr) , dimension(:), allocatable :: tau3

    complex(kind=pr) , dimension(:), allocatable :: tau4

    complex(kind=pr) :: tau5

    complex(kind=pr) , dimension(:), allocatable :: tau6

    complex(kind=pr) , dimension(:), allocatable :: tau7

    complex(kind=pr) :: tau8

    complex(kind=pr) , dimension(:), allocatable :: tau9

    complex(kind=pr) , dimension(:), allocatable :: tau10

    complex(kind=pr) :: tau11

    complex(kind=pr) , dimension(:), allocatable :: tau12

    complex(kind=pr) , dimension(:), allocatable :: tau13

    complex(kind=pr) :: tau14

    complex(kind=pr) , dimension(:), allocatable :: tau15

    complex(kind=pr) :: tau16

    complex(kind=pr) , dimension(:), allocatable :: tau17

    complex(kind=pr) , dimension(:), allocatable :: tau18

    complex(kind=pr) :: tau19

    complex(kind=pr) , dimension(:), allocatable :: tau20

    complex(kind=pr) , dimension(:), allocatable :: tau21

    complex(kind=pr) :: tau22

    complex(kind=pr) , dimension(:), allocatable :: tau23

    complex(kind=pr) :: tau24

    complex(kind=pr) , dimension(:), allocatable :: tau25

    complex(kind=pr) :: tau26

    complex(kind=pr) , dimension(:), allocatable :: tau27

    complex(kind=pr) :: tau28

    complex(kind=pr) , dimension(:), allocatable :: tau29

    complex(kind=pr) :: tau30

    complex(kind=pr) , dimension(:), allocatable :: tau31

    complex(kind=pr) :: tau32

    complex(kind=pr) , dimension(:), allocatable :: tau33

    complex(kind=pr) , dimension(:), allocatable :: tau34

    complex(kind=pr) :: tau35

    complex(kind=pr) , dimension(:), allocatable :: tau36

    complex(kind=pr) :: tau37

    complex(kind=pr) , dimension(:), allocatable :: tau38

    complex(kind=pr) , dimension(:), allocatable :: tau39

    complex(kind=pr) :: tau40

    complex(kind=pr) , dimension(:), allocatable :: tau41

    complex(kind=pr) , dimension(:), allocatable :: tau42

    complex(kind=pr) , dimension(:), allocatable :: tau43

    complex(kind=pr) :: tau44

    complex(kind=pr) , dimension(:), allocatable :: tau45

    complex(kind=pr) , dimension(:), allocatable :: tau46

    complex(kind=pr) :: tau47

    complex(kind=pr) , dimension(:), allocatable :: tau48

    complex(kind=pr) , dimension(:), allocatable :: tau49

    complex(kind=pr) :: tau50

    complex(kind=pr) , dimension(:), allocatable :: tau51

    complex(kind=pr) :: tau52

    complex(kind=pr) , dimension(:), allocatable :: tau53

    complex(kind=pr) , dimension(:), allocatable :: tau54

    complex(kind=pr) :: tau55

    complex(kind=pr) , dimension(:), allocatable :: tau56

    complex(kind=pr) , dimension(:), allocatable :: tau57

    complex(kind=pr) , dimension(:), allocatable :: tau58

    complex(kind=pr) :: tau59

    complex(kind=pr) , dimension(:), allocatable :: tau60

    complex(kind=pr) , dimension(:), allocatable :: tau61

    complex(kind=pr) :: tau62

    complex(kind=pr) , dimension(:), allocatable :: tau63

    complex(kind=pr) , dimension(:), allocatable :: tau64

    complex(kind=pr) , dimension(:), allocatable :: tau65

    complex(kind=pr) :: tau66

    complex(kind=pr) , dimension(:), allocatable :: tau67

    complex(kind=pr) , dimension(:), allocatable :: tau68

    complex(kind=pr) , dimension(:), allocatable :: tau69

    complex(kind=pr) :: tau70

    complex(kind=pr) , dimension(:), allocatable :: tau71

    complex(kind=pr) , dimension(:), allocatable :: tau72

    complex(kind=pr) , dimension(:), allocatable :: tau73

    complex(kind=pr) :: tau74

    complex(kind=pr) , dimension(:), allocatable :: tau75

    complex(kind=pr) , dimension(:), allocatable :: tau76

    complex(kind=pr) :: tau77

    complex(kind=pr) , dimension(:), allocatable :: tau78

    complex(kind=pr) , dimension(:), allocatable :: tau79

    complex(kind=pr) :: tau80

    complex(kind=pr) , dimension(:), allocatable :: tau81

    complex(kind=pr) , dimension(:), allocatable :: tau82

    complex(kind=pr) :: tau83

    complex(kind=pr) , dimension(:), allocatable :: tau84

    complex(kind=pr) :: tau85

    complex(kind=pr) , dimension(:), allocatable :: tau86

    complex(kind=pr) :: tau87

    complex(kind=pr) :: tau88

    complex(kind=pr) , dimension(:), allocatable :: tau89

    complex(kind=pr) , dimension(:), allocatable :: tau90

    complex(kind=pr) , dimension(:), allocatable :: tau91

    complex(kind=pr) :: tau92

    complex(kind=pr) , dimension(:), allocatable :: tau93

    complex(kind=pr) , dimension(:), allocatable :: tau94

    complex(kind=pr) , dimension(:), allocatable :: tau95

    complex(kind=pr) , dimension(:), allocatable :: tau96

    complex(kind=pr) :: tau97

    complex(kind=pr) , dimension(:), allocatable :: tau98

    complex(kind=pr) :: tau99

    complex(kind=pr) :: tau100

    complex(kind=pr) :: tau101

    complex(kind=pr) , dimension(:), allocatable :: tau102

    complex(kind=pr) :: tau103

    complex(kind=pr) :: tau104

    complex(kind=pr) , dimension(:), allocatable :: tau105

    complex(kind=pr) :: tau106

    complex(kind=pr) , dimension(:), allocatable :: tau107

    complex(kind=pr) , dimension(:), allocatable :: tau108

    complex(kind=pr) , dimension(:), allocatable :: tau109

    complex(kind=pr) :: tau110

    complex(kind=pr) , dimension(:), allocatable :: tau111

    complex(kind=pr) , dimension(:), allocatable :: tau112

    complex(kind=pr) , dimension(:), allocatable :: tau113

    complex(kind=pr) :: tau114

    complex(kind=pr) , dimension(:), allocatable :: tau115

    complex(kind=pr) , dimension(:), allocatable :: tau116

    complex(kind=pr) :: tau117

    complex(kind=pr) :: tau118

    complex(kind=pr) , dimension(:), allocatable :: tau119

    complex(kind=pr) , dimension(:), allocatable :: tau120

    complex(kind=pr) :: tau121

    complex(kind=pr) :: tau122

    complex(kind=pr) , dimension(:), allocatable :: tau123

    complex(kind=pr) :: tau124

    complex(kind=pr) , dimension(:), allocatable :: tau125

    complex(kind=pr) , dimension(:), allocatable :: tau126

    complex(kind=pr) , dimension(:), allocatable :: tau127

    complex(kind=pr) , dimension(:), allocatable :: tau128

    complex(kind=pr) :: tau129

    complex(kind=pr) , dimension(:), allocatable :: tau130

    complex(kind=pr) , dimension(:), allocatable :: tau131

    complex(kind=pr) , dimension(:), allocatable :: tau132

    complex(kind=pr) , dimension(:), allocatable :: tau133

    complex(kind=pr) :: tau134

    complex(kind=pr) , dimension(:), allocatable :: tau135

    complex(kind=pr) :: tau136

    complex(kind=pr) :: tau137

    complex(kind=pr) , dimension(:), allocatable :: tau138

    complex(kind=pr) :: tau139

    complex(kind=pr) , dimension(:), allocatable :: tau140

    complex(kind=pr) :: tau141

    complex(kind=pr) , dimension(:), allocatable :: tau142

    complex(kind=pr) :: tau143

    complex(kind=pr) :: tau144

    complex(kind=pr) , dimension(:), allocatable :: tau145

    complex(kind=pr) :: tau146

    complex(kind=pr) :: tau147

    complex(kind=pr) , dimension(:), allocatable :: tau148

    complex(kind=pr) :: tau149

    complex(kind=pr) , dimension(:), allocatable :: tau150

    complex(kind=pr) :: tau151

    complex(kind=pr) , dimension(:), allocatable :: tau152

    complex(kind=pr) :: tau153

    complex(kind=pr) , dimension(:), allocatable :: tau154

    complex(kind=pr) , dimension(:), allocatable :: tau155

    complex(kind=pr) :: tau156

    complex(kind=pr) , dimension(:), allocatable :: tau157

    complex(kind=pr) , dimension(:), allocatable :: tau158

    complex(kind=pr) , dimension(:), allocatable :: tau159

    complex(kind=pr) , dimension(:), allocatable :: tau160

    complex(kind=pr) :: tau161

    complex(kind=pr) , dimension(:), allocatable :: tau162

    complex(kind=pr) , dimension(:), allocatable :: tau163

    complex(kind=pr) , dimension(:), allocatable :: tau164

    complex(kind=pr) :: tau165

    complex(kind=pr) , dimension(:), allocatable :: tau166

    complex(kind=pr) , dimension(:), allocatable :: tau167

    complex(kind=pr) :: tau168

    complex(kind=pr) , dimension(:), allocatable :: tau169

    complex(kind=pr) , dimension(:), allocatable :: tau170

    complex(kind=pr) , dimension(:), allocatable :: tau171

    complex(kind=pr) :: tau172

    complex(kind=pr) , dimension(:), allocatable :: tau173

    complex(kind=pr) , dimension(:), allocatable :: tau174

    complex(kind=pr) :: tau175

    complex(kind=pr) :: tau176

    complex(kind=pr) , dimension(:), allocatable :: tau177

    complex(kind=pr) , dimension(:), allocatable :: tau178

    complex(kind=pr) :: tau179

    complex(kind=pr) :: tau180

    complex(kind=pr) , dimension(:), allocatable :: tau181

    complex(kind=pr) , dimension(:), allocatable :: tau182

    complex(kind=pr) :: tau183

    complex(kind=pr) , dimension(:), allocatable :: tau184

    complex(kind=pr) , dimension(:), allocatable :: tau185

    complex(kind=pr) :: tau186

    complex(kind=pr) , dimension(:), allocatable :: tau187

    complex(kind=pr) , dimension(:), allocatable :: tau188

    complex(kind=pr) :: tau189

    complex(kind=pr) , dimension(:), allocatable :: tau190

    complex(kind=pr) , dimension(:), allocatable :: tau191

    complex(kind=pr) :: tau192

    complex(kind=pr) , dimension(:), allocatable :: tau193

    complex(kind=pr) :: tau194

    complex(kind=pr) :: tau195

    complex(kind=pr) , dimension(:), allocatable :: tau196

    complex(kind=pr) , dimension(:, :), allocatable :: tau197

    complex(kind=pr) , dimension(:), allocatable :: tau198

    complex(kind=pr) :: tau199

    complex(kind=pr) , dimension(:), allocatable :: tau200

    complex(kind=pr) :: tau201

    complex(kind=pr) , dimension(:), allocatable :: tau202

    complex(kind=pr) :: tau203

    complex(kind=pr) , dimension(:), allocatable :: tau204

    complex(kind=pr) :: tau205

    complex(kind=pr) , dimension(:), allocatable :: tau206

    complex(kind=pr) :: tau207

    complex(kind=pr) :: tau208

    complex(kind=pr) , dimension(:), allocatable :: tau209

    complex(kind=pr) , dimension(:), allocatable :: tau210

    complex(kind=pr) :: tau211

    complex(kind=pr) , dimension(:), allocatable :: tau212

    complex(kind=pr) , dimension(:), allocatable :: tau213

    complex(kind=pr) :: tau214

    complex(kind=pr) , dimension(:), allocatable :: tau215

    complex(kind=pr) :: tau216

    complex(kind=pr) , dimension(:), allocatable :: tau217

    complex(kind=pr) , dimension(:), allocatable :: tau218

    complex(kind=pr) , dimension(:), allocatable :: tau219

    complex(kind=pr) :: tau220

    complex(kind=pr) , dimension(:), allocatable :: tau221

    complex(kind=pr) , dimension(:), allocatable :: tau222

    complex(kind=pr) :: tau223

    complex(kind=pr) , dimension(:), allocatable :: tau224

    complex(kind=pr) , dimension(:), allocatable :: tau225

    complex(kind=pr) :: tau226

    complex(kind=pr) , dimension(:), allocatable :: tau227

    complex(kind=pr) , dimension(:), allocatable :: tau228

    complex(kind=pr) :: tau229

    complex(kind=pr) , dimension(:), allocatable :: tau230

    complex(kind=pr) :: tau231

    complex(kind=pr) , dimension(:), allocatable :: tau232

    complex(kind=pr) , dimension(:), allocatable :: tau233

    complex(kind=pr) :: tau234

    complex(kind=pr) , dimension(:), allocatable :: tau235

    complex(kind=pr) :: tau236

    complex(kind=pr) , dimension(:), allocatable :: tau237

    complex(kind=pr) :: tau238

    complex(kind=pr) , dimension(:), allocatable :: tau239

    complex(kind=pr) :: tau240

    complex(kind=pr) , dimension(:), allocatable :: tau241

    complex(kind=pr) , dimension(:), allocatable :: tau242

    complex(kind=pr) , dimension(:, :), allocatable :: tau243

    complex(kind=pr) , dimension(:), allocatable :: tau244

    complex(kind=pr) :: tau245

    complex(kind=pr) , dimension(:), allocatable :: tau246

    complex(kind=pr) :: tau247

    complex(kind=pr) , dimension(:), allocatable :: tau248

    complex(kind=pr) , dimension(:), allocatable :: tau249

    complex(kind=pr) :: tau250

    complex(kind=pr) :: tau251

    complex(kind=pr) , dimension(:), allocatable :: tau252

    complex(kind=pr) :: tau253

    complex(kind=pr) , dimension(:), allocatable :: tau254

    complex(kind=pr) , dimension(:), allocatable :: tau255

    complex(kind=pr) :: tau256

    complex(kind=pr) :: tau257

    complex(kind=pr) , dimension(:), allocatable :: tau258

    complex(kind=pr) , dimension(:), allocatable :: tau259

    complex(kind=pr) , dimension(:), allocatable :: tau260

    complex(kind=pr) , dimension(:), allocatable :: tau261

    complex(kind=pr) :: tau262

    complex(kind=pr) , dimension(:), allocatable :: tau263

    complex(kind=pr) :: tau264

    complex(kind=pr) :: tau265

    complex(kind=pr) , dimension(:), allocatable :: tau266

    complex(kind=pr) :: tau267

    complex(kind=pr) :: tau268

    complex(kind=pr) , dimension(:), allocatable :: tau269

    complex(kind=pr) , dimension(:), allocatable :: tau270

    complex(kind=pr) , dimension(:), allocatable :: tau271

    complex(kind=pr) , dimension(:), allocatable :: tau272

    complex(kind=pr) :: tau273

    complex(kind=pr) , dimension(:), allocatable :: tau274

    complex(kind=pr) :: tau275

    complex(kind=pr) , dimension(:), allocatable :: tau276

    complex(kind=pr) , dimension(:), allocatable :: tau277

    complex(kind=pr) :: tau278

    complex(kind=pr) , dimension(:), allocatable :: tau279

    complex(kind=pr) , dimension(:), allocatable :: tau280

    complex(kind=pr) , dimension(:), allocatable :: tau281

    complex(kind=pr) :: tau282

    complex(kind=pr) , dimension(:), allocatable :: tau283

    complex(kind=pr) , dimension(:), allocatable :: tau284

    complex(kind=pr) , dimension(:), allocatable :: tau285

    complex(kind=pr) , dimension(:), allocatable :: tau286

    complex(kind=pr) :: tau287

    complex(kind=pr) , dimension(:), allocatable :: tau288

    complex(kind=pr) , dimension(:), allocatable :: tau289

    complex(kind=pr) :: tau290

    complex(kind=pr) , dimension(:), allocatable :: tau291

    complex(kind=pr) , dimension(:), allocatable :: tau292

    complex(kind=pr) :: tau293

    complex(kind=pr) , dimension(:), allocatable :: tau294

    complex(kind=pr) :: tau295

    complex(kind=pr) , dimension(:), allocatable :: tau296

    complex(kind=pr) , dimension(:), allocatable :: tau297

    complex(kind=pr) :: tau298

    complex(kind=pr) , dimension(:), allocatable :: tau299

    complex(kind=pr) , dimension(:), allocatable :: tau300

    complex(kind=pr) , dimension(:), allocatable :: tau301

    complex(kind=pr) , dimension(:), allocatable :: tau302

    complex(kind=pr) :: tau303

    complex(kind=pr) , dimension(:), allocatable :: tau304

    complex(kind=pr) , dimension(:), allocatable :: tau305

    complex(kind=pr) :: tau306

    complex(kind=pr) , dimension(:), allocatable :: tau307

    complex(kind=pr) :: tau308

    complex(kind=pr) , dimension(:), allocatable :: tau309

    complex(kind=pr) , dimension(:), allocatable :: tau310

    complex(kind=pr) :: tau311

    complex(kind=pr) , dimension(:), allocatable :: tau312

    complex(kind=pr) , dimension(:), allocatable :: tau313

    complex(kind=pr) , dimension(:), allocatable :: tau314

    complex(kind=pr) :: tau315

    complex(kind=pr) , dimension(:), allocatable :: tau316

    complex(kind=pr) :: tau317

    complex(kind=pr) , dimension(:), allocatable :: tau318

    complex(kind=pr) , dimension(:), allocatable :: tau319

    complex(kind=pr) :: tau320

    complex(kind=pr) , dimension(:), allocatable :: tau321

    complex(kind=pr) :: tau322

    complex(kind=pr) , dimension(:), allocatable :: tau323

    complex(kind=pr) , dimension(:), allocatable :: tau324

    complex(kind=pr) :: tau325

    complex(kind=pr) , dimension(:), allocatable :: tau326

    complex(kind=pr) , dimension(:), allocatable :: tau327

    complex(kind=pr) :: tau328

    complex(kind=pr) , dimension(:), allocatable :: tau329

    complex(kind=pr) , dimension(:), allocatable :: tau330

    complex(kind=pr) , dimension(:), allocatable :: tau331

    complex(kind=pr) :: tau332

    complex(kind=pr) , dimension(:), allocatable :: tau333

    complex(kind=pr) , dimension(:), allocatable :: tau334

    complex(kind=pr) :: tau335

    complex(kind=pr) , dimension(:), allocatable :: tau336

    complex(kind=pr) , dimension(:), allocatable :: tau337

    complex(kind=pr) , dimension(:), allocatable :: tau338

    complex(kind=pr) :: tau339

    complex(kind=pr) , dimension(:), allocatable :: tau340

    complex(kind=pr) :: tau341

    complex(kind=pr) , dimension(:), allocatable :: tau342

    complex(kind=pr) , dimension(:), allocatable :: tau343

    complex(kind=pr) , dimension(:), allocatable :: tau344

    complex(kind=pr) :: tau345

    complex(kind=pr) , dimension(:), allocatable :: tau346

    complex(kind=pr) :: tau347

    complex(kind=pr) , dimension(:), allocatable :: tau348

    complex(kind=pr) :: tau349

    complex(kind=pr) , dimension(:), allocatable :: tau350

    complex(kind=pr) :: tau351

    complex(kind=pr) , dimension(:), allocatable :: tau352

    complex(kind=pr) , dimension(:), allocatable :: tau353

    complex(kind=pr) :: tau354

    complex(kind=pr) , dimension(:), allocatable :: tau355

    complex(kind=pr) , dimension(:), allocatable :: tau356

    complex(kind=pr) , dimension(:), allocatable :: tau357

    complex(kind=pr) :: tau358

    complex(kind=pr) , dimension(:), allocatable :: tau359

    complex(kind=pr) , dimension(:), allocatable :: tau360

    complex(kind=pr) :: tau361

    complex(kind=pr) , dimension(:), allocatable :: tau362

    complex(kind=pr) , dimension(:), allocatable :: tau363

    complex(kind=pr) , dimension(:), allocatable :: tau364

    complex(kind=pr) :: tau365

    complex(kind=pr) , dimension(:), allocatable :: tau366

    complex(kind=pr) :: tau367

    complex(kind=pr) :: tau368

    complex(kind=pr) , dimension(:), allocatable :: tau369

    complex(kind=pr) , dimension(:), allocatable :: tau370

    complex(kind=pr) :: tau371

    complex(kind=pr) , dimension(:), allocatable :: tau372

    complex(kind=pr) , dimension(:), allocatable :: tau373

    complex(kind=pr) , dimension(:), allocatable :: tau374

    complex(kind=pr) :: tau375

    complex(kind=pr) , dimension(:), allocatable :: tau376

    complex(kind=pr) , dimension(:), allocatable :: tau377

    complex(kind=pr) :: tau378

    complex(kind=pr) , dimension(:), allocatable :: tau379

    complex(kind=pr) , dimension(:), allocatable :: tau380

    complex(kind=pr) :: tau381

    complex(kind=pr) :: tau382

    complex(kind=pr) , dimension(:), allocatable :: tau383

    complex(kind=pr) , dimension(:), allocatable :: tau384

    complex(kind=pr) :: tau385

    complex(kind=pr) , dimension(:), allocatable :: tau386

    complex(kind=pr) , dimension(:), allocatable :: tau387

    complex(kind=pr) , dimension(:), allocatable :: tau388

    complex(kind=pr) , dimension(:), allocatable :: tau389

    complex(kind=pr) , dimension(:), allocatable :: tau390

    complex(kind=pr) :: tau391

    complex(kind=pr) , dimension(:), allocatable :: tau392

    complex(kind=pr) :: tau393

    complex(kind=pr) :: tau394

    complex(kind=pr) , dimension(:), allocatable :: tau395

    complex(kind=pr) :: tau396

    complex(kind=pr) , dimension(:), allocatable :: tau397

    complex(kind=pr) :: tau398

    complex(kind=pr) , dimension(:), allocatable :: tau399

    complex(kind=pr) :: tau400

    complex(kind=pr) , dimension(:), allocatable :: tau401

    complex(kind=pr) :: tau402

    complex(kind=pr) :: tau403

    complex(kind=pr) , dimension(:), allocatable :: tau404

    complex(kind=pr) :: tau405

    complex(kind=pr) , dimension(:), allocatable :: tau406

    complex(kind=pr) , dimension(:), allocatable :: tau407

    complex(kind=pr) , dimension(:), allocatable :: tau408

    complex(kind=pr) , dimension(:), allocatable :: tau409

    complex(kind=pr) , dimension(:), allocatable :: tau410

    complex(kind=pr) :: tau411

    complex(kind=pr) , dimension(:), allocatable :: tau412

    complex(kind=pr) , dimension(:), allocatable :: tau413

    complex(kind=pr) :: tau414

    complex(kind=pr) , dimension(:), allocatable :: tau415

    complex(kind=pr) , dimension(:), allocatable :: tau416

    complex(kind=pr) , dimension(:), allocatable :: tau417

    complex(kind=pr) :: tau418

    complex(kind=pr) , dimension(:), allocatable :: tau419

    complex(kind=pr) , dimension(:), allocatable :: tau420

    complex(kind=pr) :: tau421

    complex(kind=pr) , dimension(:), allocatable :: tau422

    complex(kind=pr) , dimension(:), allocatable :: tau423

    complex(kind=pr) :: tau424

    complex(kind=pr) , dimension(:), allocatable :: tau425

    complex(kind=pr) , dimension(:), allocatable :: tau426

    complex(kind=pr) , dimension(:), allocatable :: tau427

    complex(kind=pr) :: tau428

    complex(kind=pr) , dimension(:), allocatable :: tau429

    complex(kind=pr) :: tau430

    complex(kind=pr) , dimension(:), allocatable :: tau431

    complex(kind=pr) :: tau432

    complex(kind=pr) , dimension(:), allocatable :: tau433

    complex(kind=pr) :: tau434

    complex(kind=pr) , dimension(:), allocatable :: tau435

    complex(kind=pr) :: tau436

    complex(kind=pr) , dimension(:), allocatable :: tau437

    complex(kind=pr) :: tau438

    complex(kind=pr) , dimension(:), allocatable :: tau439

    complex(kind=pr) :: tau440

    complex(kind=pr) , dimension(:), allocatable :: tau441

    complex(kind=pr) :: tau442

    complex(kind=pr) , dimension(:), allocatable :: tau443

    complex(kind=pr) :: tau444

    complex(kind=pr) , dimension(:), allocatable :: tau445

    complex(kind=pr) :: tau446

    complex(kind=pr) , dimension(:), allocatable :: tau447

    complex(kind=pr) , dimension(:, :), allocatable :: tau448

    complex(kind=pr) , dimension(:), allocatable :: tau449

    complex(kind=pr) , dimension(:), allocatable :: tau450

    complex(kind=pr) :: tau451

    complex(kind=pr) , dimension(:), allocatable :: tau452

    complex(kind=pr) :: tau453

    complex(kind=pr) , dimension(:), allocatable :: tau454

    complex(kind=pr) :: tau455

    complex(kind=pr) , dimension(:), allocatable :: tau456

    complex(kind=pr) :: tau457

    complex(kind=pr) , dimension(:), allocatable :: tau458

    complex(kind=pr) :: tau459

    complex(kind=pr) , dimension(:), allocatable :: tau460

    complex(kind=pr) , dimension(:), allocatable :: tau461

    complex(kind=pr) :: tau462

    complex(kind=pr) , dimension(:), allocatable :: tau463

    complex(kind=pr) , dimension(:), allocatable :: tau464

    complex(kind=pr) , dimension(:), allocatable :: tau465

    complex(kind=pr) , dimension(:), allocatable :: tau466

    complex(kind=pr) , dimension(:), allocatable :: tau467

    complex(kind=pr) :: tau468

    complex(kind=pr) , dimension(:), allocatable :: tau469

    complex(kind=pr) , dimension(:), allocatable :: tau470

    complex(kind=pr) , dimension(:), allocatable :: tau471

    complex(kind=pr) :: tau472

    complex(kind=pr) , dimension(:), allocatable :: tau473

    complex(kind=pr) , dimension(:), allocatable :: tau474

    complex(kind=pr) , dimension(:), allocatable :: tau475

    complex(kind=pr) :: tau476

    complex(kind=pr) , dimension(:), allocatable :: tau477

    complex(kind=pr) :: tau478

    complex(kind=pr) , dimension(:), allocatable :: tau479

    complex(kind=pr) :: tau480

    complex(kind=pr) , dimension(:), allocatable :: tau481

    complex(kind=pr) , dimension(:), allocatable :: tau482

    complex(kind=pr) :: tau483

    complex(kind=pr) , dimension(:), allocatable :: tau484

    complex(kind=pr) , dimension(:), allocatable :: tau485

    complex(kind=pr) , dimension(:), allocatable :: tau486

    complex(kind=pr) :: tau487

    complex(kind=pr) , dimension(:), allocatable :: tau488

    complex(kind=pr) , dimension(:), allocatable :: tau489

    complex(kind=pr) , dimension(:), allocatable :: tau490

    complex(kind=pr) :: tau491

    complex(kind=pr) , dimension(:), allocatable :: tau492

    complex(kind=pr) , dimension(:), allocatable :: tau493

    complex(kind=pr) , dimension(:), allocatable :: tau494

    complex(kind=pr) :: tau495

    complex(kind=pr) , dimension(:), allocatable :: tau496

    complex(kind=pr) :: tau497

    complex(kind=pr) , dimension(:), allocatable :: tau498

    complex(kind=pr) , dimension(:), allocatable :: tau499

    complex(kind=pr) , dimension(:), allocatable :: tau500

    complex(kind=pr) :: tau501

    complex(kind=pr) , dimension(:), allocatable :: tau502

    complex(kind=pr) , dimension(:), allocatable :: tau503

    complex(kind=pr) :: tau504

    complex(kind=pr) , dimension(:), allocatable :: tau505

    complex(kind=pr) , dimension(:), allocatable :: tau506

    complex(kind=pr) , dimension(:), allocatable :: tau507

    complex(kind=pr) :: tau508

    complex(kind=pr) , dimension(:), allocatable :: tau509

    complex(kind=pr) , dimension(:, :), allocatable :: tau510

    complex(kind=pr) , dimension(:, :), allocatable :: tau511

    complex(kind=pr) , dimension(:, :), allocatable :: tau512

    complex(kind=pr) , dimension(:, :), allocatable :: tau513

    complex(kind=pr) , dimension(:, :), allocatable :: tau514

    complex(kind=pr) , dimension(:, :), allocatable :: tau515

    complex(kind=pr) , dimension(:, :), allocatable :: tau516

    complex(kind=pr) , dimension(:, :), allocatable :: tau517

    complex(kind=pr) , dimension(:, :), allocatable :: tau518

    complex(kind=pr) , dimension(:, :), allocatable :: tau519

    complex(kind=pr) , dimension(:, :), allocatable :: tau520

    complex(kind=pr) , dimension(:, :), allocatable :: tau521

    complex(kind=pr) , dimension(:, :), allocatable :: tau522

    complex(kind=pr) , dimension(:, :), allocatable :: tau523

    complex(kind=pr) , dimension(:, :), allocatable :: tau524

    !$omp parallel default(shared)

    !$omp single
    tau0 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau0)
    
    do p=1, NAO
        tau0 = tau0 + ( &
            v(p)**4&
        )
    end do
    
    !$omp end do

    allocate(tau1(NAO))
    !$omp single
    tau1 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau1(p) = tau1(p) + ( &
            tau0 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    SpSq = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(q) * tau1(p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau1)

    !$omp single
    tau2 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau2)
    
    do p=1, NAO
        tau2 = tau2 + ( &
            u(p)**2 * v(p)**2&
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
            tau2 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(q) * tau3(p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau3)

    allocate(tau4(NAO))
    !$omp single
    tau4 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau4(p) = tau4(p) + ( &
            t2(p, p) * z2(p, p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau5 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau5)
    
    do p=1, NAO
        tau5 = tau5 + ( &
            u(p)**2 * tau4(p)&
        )
    end do
    
    !$omp end do

    allocate(tau6(NAO))
    !$omp single
    tau6 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau6(p) = tau6(p) + ( &
            tau5 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau6(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    tau24 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau24)
    
    do p=1, NAO
        tau24 = tau24 + ( &
            v(p)**4 * tau4(p)&
        )
    end do
    
    !$omp end do

    allocate(tau25(NAO))
    !$omp single
    tau25 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau25(p) = tau25(p) + ( &
            tau24 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau25(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau25)

    !$omp single
    tau28 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau28)
    
    do p=1, NAO
        tau28 = tau28 + ( &
            u(p)**4 * tau4(p)&
        )
    end do
    
    !$omp end do

    allocate(tau29(NAO))
    !$omp single
    tau29 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau29(p) = tau29(p) + ( &
            tau28 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau29(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau29)

    !$omp single
    tau30 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau30)
    
    do p=1, NAO
        tau30 = tau30 + ( &
            v(p)**2 * tau4(p)&
        )
    end do
    
    !$omp end do

    allocate(tau31(NAO))
    !$omp single
    tau31 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau31(p) = tau31(p) + ( &
            tau30 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau31(q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau57(NAO))
    !$omp single
    tau57 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau57(p) = tau57(p) + ( &
            tau4(p) * v(p)&
        )
    
    end do
    !$omp end do

    allocate(tau58(NAO))
    !$omp single
    tau58 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau58(p) = tau58(p) + ( &
            tau57(p) * u(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau59 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau59)
    
    do p=1, NAO
        tau59 = tau59 + ( &
            t1(p) * tau58(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau58)

    allocate(tau60(NAO))
    !$omp single
    tau60 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau60(p) = tau60(p) + ( &
            tau59 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau60(q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau61(NAO))
    !$omp single
    tau61 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau61(p) = tau61(p) + ( &
            t1(p) * tau57(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau62 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau62)
    
    do p=1, NAO
        tau62 = tau62 + ( &
            u(p)**3 * tau61(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau61)

    allocate(tau63(NAO))
    !$omp single
    tau63 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau63(p) = tau63(p) + ( &
            tau62 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau63(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau63)

    !$omp single
    tau402 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau402)
    
    do p=1, NAO
        tau402 = tau402 + ( &
            tau57(p) * u(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau57)

    allocate(tau64(NAO))
    !$omp single
    tau64 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau64(p) = tau64(p) + ( &
            tau4(p) * u(p)&
        )
    
    end do
    !$omp end do

    allocate(tau65(NAO))
    !$omp single
    tau65 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau65(p) = tau65(p) + ( &
            t1(p) * tau64(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau64)

    !$omp single
    tau66 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau66)
    
    do p=1, NAO
        tau66 = tau66 + ( &
            v(p)**3 * tau65(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau65)

    allocate(tau67(NAO))
    !$omp single
    tau67 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau67(p) = tau67(p) + ( &
            tau66 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau67(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau67)

    allocate(tau313(NAO))
    !$omp single
    tau313 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau313(p) = tau313(p) + ( &
            v(p)**2 * tau4(p)&
        )
    
    end do
    !$omp end do

    allocate(tau314(NAO))
    !$omp single
    tau314 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau314(p) = tau314(p) + ( &
                tau313(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau313)

    !$omp single
    tau315 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau315)
    
    do p=1, NAO
        tau315 = tau315 + ( &
            u(p)**2 * tau314(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau314)

    allocate(tau316(NAO))
    !$omp single
    tau316 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau316(p) = tau316(p) + ( &
            tau315 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                3 * one(p) * tau316(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau316)

    allocate(tau380(NAO))
    !$omp single
    tau380 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau380(p) = tau380(p) + ( &
            t1(p) * tau4(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau4)

    !$omp single
    tau381 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau381)
    
    do p=1, NAO
        tau381 = tau381 + ( &
            u(p)**2 * tau380(p)&
        )
    end do
    
    !$omp end do

    !$omp single
    tau393 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau393)
    
    do p=1, NAO
        tau393 = tau393 + ( &
            v(p)**2 * tau380(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau380)

    allocate(tau7(NAO))
    !$omp single
    tau7 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau7(p) = tau7(p) + ( &
            t1(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau8 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau8)
    
    do p=1, NAO
        tau8 = tau8 + ( &
            u(p)**3 * tau7(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau7)

    allocate(tau9(NAO))
    !$omp single
    tau9 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau9(p) = tau9(p) + ( &
            tau8 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau9(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau9)

    allocate(tau10(NAO))
    !$omp single
    tau10 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau10(p) = tau10(p) + ( &
            v(p) * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau11 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau11)
    
    do p=1, NAO
        tau11 = tau11 + ( &
            u(p)**3 * tau10(p)&
        )
    end do
    
    !$omp end do

    allocate(tau12(NAO))
    !$omp single
    tau12 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau12(p) = tau12(p) + ( &
            tau11 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau12(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau12)

    !$omp single
    tau37 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau37)
    
    do p=1, NAO
        tau37 = tau37 + ( &
            tau10(p) * u(p)&
        )
    end do
    
    !$omp end do

    allocate(tau38(NAO))
    !$omp single
    tau38 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau38(p) = tau38(p) + ( &
            tau37 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau38(q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau39(NAO))
    !$omp single
    tau39 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau39(p) = tau39(p) + ( &
            tau10(p) * u(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau40 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau40)
    
    do p=1, NAO
        tau40 = tau40 + ( &
            t1(p)**2 * tau39(p)&
        )
    end do
    
    !$omp end do

    allocate(tau41(NAO))
    !$omp single
    tau41 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau41(p) = tau41(p) + ( &
            tau40 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau41(q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau324(NAO))
    !$omp single
    tau324 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau324(p) = tau324(p) + ( &
                tau39(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp single
    tau325 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau325)
    
    do p=1, NAO
        tau325 = tau325 + ( &
            u(p)**2 * tau324(p)&
        )
    end do
    
    !$omp end do

    allocate(tau326(NAO))
    !$omp single
    tau326 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau326(p) = tau326(p) + ( &
            tau325 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau326(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau326)

    !$omp single
    tau351 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau351)
    
    do p=1, NAO
        tau351 = tau351 + ( &
            v(p)**2 * tau324(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau324)

    allocate(tau352(NAO))
    !$omp single
    tau352 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau352(p) = tau352(p) + ( &
            tau351 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau352(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau352)

    !$omp single
    tau367 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau367)
    
    do p=1, NAO
        tau367 = tau367 + ( &
            t1(p) * tau39(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau39)

    allocate(tau46(NAO))
    !$omp single
    tau46 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau46(p) = tau46(p) + ( &
            u(p)**3 * tau10(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau10)

    !$omp single
    tau47 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau47)
    
    do p=1, NAO
        tau47 = tau47 + ( &
            t1(p)**2 * tau46(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau46)

    allocate(tau48(NAO))
    !$omp single
    tau48 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau48(p) = tau48(p) + ( &
            tau47 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau48(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau48)

    allocate(tau13(NAO))
    !$omp single
    tau13 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau13(p) = tau13(p) + ( &
            t1(p) * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau14 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau14)
    
    do p=1, NAO
        tau14 = tau14 + ( &
            u(p)**4 * tau13(p)&
        )
    end do
    
    !$omp end do

    allocate(tau15(NAO))
    !$omp single
    tau15 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau15(p) = tau15(p) + ( &
            tau14 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau15(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau15)

    !$omp single
    tau16 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau16)
    
    do p=1, NAO
        tau16 = tau16 + ( &
            v(p)**2 * tau13(p)&
        )
    end do
    
    !$omp end do

    allocate(tau17(NAO))
    !$omp single
    tau17 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau17(p) = tau17(p) + ( &
            tau16 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau17(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    tau26 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau26)
    
    do p=1, NAO
        tau26 = tau26 + ( &
            u(p)**2 * tau13(p)&
        )
    end do
    
    !$omp end do

    allocate(tau27(NAO))
    !$omp single
    tau27 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau27(p) = tau27(p) + ( &
            tau26 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau27(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    tau32 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau32)
    
    do p=1, NAO
        tau32 = tau32 + ( &
            v(p)**4 * tau13(p)&
        )
    end do
    
    !$omp end do

    allocate(tau33(NAO))
    !$omp single
    tau33 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau33(p) = tau33(p) + ( &
            tau32 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau33(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau33)

    allocate(tau343(NAO))
    !$omp single
    tau343 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau343(p) = tau343(p) + ( &
            v(p)**2 * tau13(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau13)

    allocate(tau344(NAO))
    !$omp single
    tau344 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau344(p) = tau344(p) + ( &
                tau343(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau343)

    !$omp single
    tau345 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau345)
    
    do p=1, NAO
        tau345 = tau345 + ( &
            u(p)**2 * tau344(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau344)

    allocate(tau346(NAO))
    !$omp single
    tau346 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau346(p) = tau346(p) + ( &
            tau345 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau346(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau346)

    allocate(tau18(NAO))
    !$omp single
    tau18 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau18(p) = tau18(p) + ( &
            t1(p) * u(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau19 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau19)
    
    do p=1, NAO
        tau19 = tau19 + ( &
            v(p)**3 * tau18(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau18)

    allocate(tau20(NAO))
    !$omp single
    tau20 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau20(p) = tau20(p) + ( &
            tau19 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau20(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau20)

    allocate(tau21(NAO))
    !$omp single
    tau21 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau21(p) = tau21(p) + ( &
            u(p) * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau22 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau22)
    
    do p=1, NAO
        tau22 = tau22 + ( &
            v(p)**3 * tau21(p)&
        )
    end do
    
    !$omp end do

    allocate(tau23(NAO))
    !$omp single
    tau23 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau23(p) = tau23(p) + ( &
            tau22 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau23(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau23)

    allocate(tau49(NAO))
    !$omp single
    tau49 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau49(p) = tau49(p) + ( &
            v(p)**3 * tau21(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau21)

    !$omp single
    tau50 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau50)
    
    do p=1, NAO
        tau50 = tau50 + ( &
            t1(p)**2 * tau49(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau49)

    allocate(tau51(NAO))
    !$omp single
    tau51 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau51(p) = tau51(p) + ( &
            tau50 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau51(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau51)

    allocate(tau34(NAO))
    !$omp single
    tau34 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau34(p) = tau34(p) + ( &
            u(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau35 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau35)
    
    do p=1, NAO
        tau35 = tau35 + ( &
            t1(p) * tau34(p)&
        )
    end do
    
    !$omp end do

    allocate(tau36(NAO))
    !$omp single
    tau36 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau36(p) = tau36(p) + ( &
            tau35 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau36(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    tau354 = 0.0
    !$omp end single

    !$omp single
    
    
    tau354 = tau354 + ( &
        tau35**2&
    )
    
    
    !$omp end single

    allocate(tau355(NAO))
    !$omp single
    tau355 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau355(p) = tau355(p) + ( &
            tau354 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau355(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau355)

    !$omp single
    tau398 = 0.0
    !$omp end single

    !$omp single
    
    
    tau398 = tau398 + ( &
        tau35*tau5&
    )
    
    
    !$omp end single

    allocate(tau399(NAO))
    !$omp single
    tau399 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau399(p) = tau399(p) + ( &
            tau398 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau399(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau399)

    !$omp single
    tau405 = 0.0
    !$omp end single

    !$omp single
    
    
    tau405 = tau405 + ( &
        tau16*tau35&
    )
    
    
    !$omp end single

    allocate(tau406(NAO))
    !$omp single
    tau406 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau406(p) = tau406(p) + ( &
            tau405 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau406(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau406)

    !$omp single
    tau434 = 0.0
    !$omp end single

    !$omp single
    
    
    tau434 = tau434 + ( &
        tau26*tau35&
    )
    
    
    !$omp end single

    allocate(tau435(NAO))
    !$omp single
    tau435 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau435(p) = tau435(p) + ( &
            tau434 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau435(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau435)

    !$omp single
    tau446 = 0.0
    !$omp end single

    !$omp single
    
    
    tau446 = tau446 + ( &
        tau30*tau35&
    )
    
    
    !$omp end single

    allocate(tau447(NAO))
    !$omp single
    tau447 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau447(p) = tau447(p) + ( &
            tau446 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau447(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau447)

    !$omp single
    tau453 = 0.0
    !$omp end single

    !$omp single
    
    
    tau453 = tau453 + ( &
        tau35*tau37&
    )
    
    
    !$omp end single

    allocate(tau454(NAO))
    !$omp single
    tau454 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau454(p) = tau454(p) + ( &
            tau453 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau454(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau454)

    !$omp single
    tau478 = 0.0
    !$omp end single

    !$omp single
    
    
    tau478 = tau478 + ( &
        tau35*tau40&
    )
    
    
    !$omp end single

    allocate(tau479(NAO))
    !$omp single
    tau479 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau479(p) = tau479(p) + ( &
            tau478 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau479(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau479)

    !$omp single
    tau497 = 0.0
    !$omp end single

    !$omp single
    
    
    tau497 = tau497 + ( &
        tau35*tau59&
    )
    
    
    !$omp end single

    allocate(tau498(NAO))
    !$omp single
    tau498 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau498(p) = tau498(p) + ( &
            tau497 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                4 * one(p) * tau498(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau498)

    allocate(tau126(NAO))
    !$omp single
    tau126 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau126(p) = tau126(p) + ( &
            t1(p) * tau34(p)&
        )
    
    end do
    !$omp end do

    allocate(tau127(NAO))
    !$omp single
    tau127 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau127(p) = tau127(p) + ( &
                tau126(q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau128(NAO))
    !$omp single
    tau128 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau128(p) = tau128(p) + ( &
                tau127(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp single
    tau129 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau129)
    
    do p=1, NAO
        tau129 = tau129 + ( &
            u(p)**2 * tau128(p)&
        )
    end do
    
    !$omp end do

    allocate(tau130(NAO))
    !$omp single
    tau130 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau130(p) = tau130(p) + ( &
            tau129 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau130(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau130)

    !$omp single
    tau153 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau153)
    
    do p=1, NAO
        tau153 = tau153 + ( &
            v(p)**2 * tau128(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau128)

    allocate(tau154(NAO))
    !$omp single
    tau154 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau154(p) = tau154(p) + ( &
            tau153 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau154(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau154)

    !$omp single
    tau322 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau322)
    
    do p=1, NAO
        tau322 = tau322 + ( &
            u(p)**2 * tau127(p)&
        )
    end do
    
    !$omp end do

    allocate(tau323(NAO))
    !$omp single
    tau323 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau323(p) = tau323(p) + ( &
            tau322 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau323(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau323)

    !$omp single
    tau349 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau349)
    
    do p=1, NAO
        tau349 = tau349 + ( &
            v(p)**2 * tau127(p)&
        )
    end do
    
    !$omp end do

    allocate(tau350(NAO))
    !$omp single
    tau350 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau350(p) = tau350(p) + ( &
            tau349 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau350(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau350)

    allocate(tau377(NAO))
    !$omp single
    tau377 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau377(p) = tau377(p) + ( &
            v(p)**2 * tau127(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau378 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau378)
    
    do p=1, NAO
        tau378 = tau378 + ( &
            t1(p)**2 * tau377(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau377)

    allocate(tau379(NAO))
    !$omp single
    tau379 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau379(p) = tau379(p) + ( &
            tau378 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau379(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau379)

    allocate(tau413(NAO))
    !$omp single
    tau413 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau413(p) = tau413(p) + ( &
            tau127(p) * t2(p, p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau414 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau414)
    
    do p=1, NAO
        tau414 = tau414 + ( &
            v(p)**2 * tau413(p)&
        )
    end do
    
    !$omp end do

    allocate(tau415(NAO))
    !$omp single
    tau415 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau415(p) = tau415(p) + ( &
            tau414 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau415(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau415)

    !$omp single
    tau438 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau438)
    
    do p=1, NAO
        tau438 = tau438 + ( &
            u(p)**2 * tau413(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau413)

    allocate(tau439(NAO))
    !$omp single
    tau439 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau439(p) = tau439(p) + ( &
            tau438 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau439(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau439)

    allocate(tau423(NAO))
    !$omp single
    tau423 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau423(p) = tau423(p) + ( &
            u(p)**2 * tau127(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau424 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau424)
    
    do p=1, NAO
        tau424 = tau424 + ( &
            t1(p)**2 * tau423(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau423)

    allocate(tau425(NAO))
    !$omp single
    tau425 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau425(p) = tau425(p) + ( &
            tau424 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau425(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau425)

    allocate(tau489(NAO))
    !$omp single
    tau489 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau489(p) = tau489(p) + ( &
            tau127(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau127)

    allocate(tau490(NAO))
    !$omp single
    tau490 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau490(p) = tau490(p) + ( &
            tau489(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau489)

    !$omp single
    tau491 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau491)
    
    do p=1, NAO
        tau491 = tau491 + ( &
            t1(p) * tau490(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau490)

    allocate(tau492(NAO))
    !$omp single
    tau492 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau492(p) = tau492(p) + ( &
            tau491 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                4 * one(p) * tau492(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau492)

    allocate(tau131(NAO))
    !$omp single
    tau131 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau131(p) = tau131(p) + ( &
                tau34(q) * t2(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau132(NAO))
    !$omp single
    tau132 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau132(p) = tau132(p) + ( &
                tau131(q) * z2(q, p)&
            )
        end do
    end do
    !$omp end do

    allocate(tau133(NAO))
    !$omp single
    tau133 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau133(p) = tau133(p) + ( &
            t1(p) * tau132(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau134 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau134)
    
    do p=1, NAO
        tau134 = tau134 + ( &
            v(p)**2 * tau133(p)&
        )
    end do
    
    !$omp end do

    allocate(tau135(NAO))
    !$omp single
    tau135 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau135(p) = tau135(p) + ( &
            tau134 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau135(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau135)

    !$omp single
    tau141 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau141)
    
    do p=1, NAO
        tau141 = tau141 + ( &
            u(p)**2 * tau133(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau133)

    allocate(tau142(NAO))
    !$omp single
    tau142 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau142(p) = tau142(p) + ( &
            tau141 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau142(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau142)

    allocate(tau155(NAO))
    !$omp single
    tau155 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau155(p) = tau155(p) + ( &
            tau132(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau156 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau156)
    
    do p=1, NAO
        tau156 = tau156 + ( &
            tau155(p) * u(p)&
        )
    end do
    
    !$omp end do

    allocate(tau157(NAO))
    !$omp single
    tau157 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau157(p) = tau157(p) + ( &
            tau156 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau157(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau157)

    allocate(tau167(NAO))
    !$omp single
    tau167 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau167(p) = tau167(p) + ( &
            tau155(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau155)

    !$omp single
    tau168 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau168)
    
    do p=1, NAO
        tau168 = tau168 + ( &
            t1(p)**2 * tau167(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau167)

    allocate(tau169(NAO))
    !$omp single
    tau169 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau169(p) = tau169(p) + ( &
            tau168 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau169(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau169)

    allocate(tau170(NAO))
    !$omp single
    tau170 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau170(p) = tau170(p) + ( &
            tau132(p) * t2(p, p)&
        )
    
    end do
    !$omp end do

    allocate(tau171(NAO))
    !$omp single
    tau171 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau171(p) = tau171(p) + ( &
            tau170(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau170)

    !$omp single
    tau172 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau172)
    
    do p=1, NAO
        tau172 = tau172 + ( &
            tau171(p) * u(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau171)

    allocate(tau173(NAO))
    !$omp single
    tau173 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau173(p) = tau173(p) + ( &
            tau172 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau173(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau173)

    allocate(tau506(NAO))
    !$omp single
    tau506 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau506(p) = tau506(p) + ( &
                tau132(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau132)

    allocate(tau507(NAO))
    !$omp single
    tau507 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau507(p) = tau507(p) + ( &
            tau506(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau506)

    !$omp single
    tau508 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau508)
    
    do p=1, NAO
        tau508 = tau508 + ( &
            tau507(p) * u(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau507)

    allocate(tau509(NAO))
    !$omp single
    tau509 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau509(p) = tau509(p) + ( &
            tau508 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau509(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau509)

    allocate(tau158(NAO))
    !$omp single
    tau158 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau158(p) = tau158(p) + ( &
            tau131(p) * z2(p, p)&
        )
    
    end do
    !$omp end do

    allocate(tau159(NAO))
    !$omp single
    tau159 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau159(p) = tau159(p) + ( &
                tau158(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    allocate(tau160(NAO))
    !$omp single
    tau160 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau160(p) = tau160(p) + ( &
            tau159(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau159)

    !$omp single
    tau161 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau161)
    
    do p=1, NAO
        tau161 = tau161 + ( &
            tau160(p) * u(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau160)

    allocate(tau162(NAO))
    !$omp single
    tau162 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau162(p) = tau162(p) + ( &
            tau161 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau162(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau162)

    allocate(tau384(NAO))
    !$omp single
    tau384 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau384(p) = tau384(p) + ( &
            t1(p) * tau158(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau385 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau385)
    
    do p=1, NAO
        tau385 = tau385 + ( &
            u(p)**2 * tau384(p)&
        )
    end do
    
    !$omp end do

    allocate(tau386(NAO))
    !$omp single
    tau386 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau386(p) = tau386(p) + ( &
            tau385 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau386(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau386)

    !$omp single
    tau440 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau440)
    
    do p=1, NAO
        tau440 = tau440 + ( &
            v(p)**2 * tau384(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau384)

    allocate(tau441(NAO))
    !$omp single
    tau441 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau441(p) = tau441(p) + ( &
            tau440 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau441(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau441)

    allocate(tau420(NAO))
    !$omp single
    tau420 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau420(p) = tau420(p) + ( &
            tau158(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau421 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau421)
    
    do p=1, NAO
        tau421 = tau421 + ( &
            tau420(p) * u(p)&
        )
    end do
    
    !$omp end do

    allocate(tau422(NAO))
    !$omp single
    tau422 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau422(p) = tau422(p) + ( &
            tau421 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau422(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau422)

    allocate(tau482(NAO))
    !$omp single
    tau482 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau482(p) = tau482(p) + ( &
            tau420(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau420)

    !$omp single
    tau483 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau483)
    
    do p=1, NAO
        tau483 = tau483 + ( &
            t1(p)**2 * tau482(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau482)

    allocate(tau484(NAO))
    !$omp single
    tau484 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau484(p) = tau484(p) + ( &
            tau483 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau484(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau484)

    allocate(tau493(NAO))
    !$omp single
    tau493 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau493(p) = tau493(p) + ( &
            tau158(p) * t2(p, p)&
        )
    
    end do
    !$omp end do

    deallocate(tau158)

    allocate(tau494(NAO))
    !$omp single
    tau494 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau494(p) = tau494(p) + ( &
            tau493(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau493)

    !$omp single
    tau495 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau495)
    
    do p=1, NAO
        tau495 = tau495 + ( &
            tau494(p) * u(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau494)

    allocate(tau496(NAO))
    !$omp single
    tau496 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau496(p) = tau496(p) + ( &
            tau495 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                6 * one(p) * tau496(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau496)

    allocate(tau277(NAO))
    !$omp single
    tau277 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau277(p) = tau277(p) + ( &
            tau131(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau278 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau278)
    
    do p=1, NAO
        tau278 = tau278 + ( &
            tau277(p) * u(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau277)

    allocate(tau279(NAO))
    !$omp single
    tau279 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau279(p) = tau279(p) + ( &
            tau278 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau279(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau279)

    allocate(tau334(NAO))
    !$omp single
    tau334 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau334(p) = tau334(p) + ( &
            tau131(p) * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau335 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau335)
    
    do p=1, NAO
        tau335 = tau335 + ( &
            v(p)**2 * tau334(p)&
        )
    end do
    
    !$omp end do

    allocate(tau336(NAO))
    !$omp single
    tau336 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau336(p) = tau336(p) + ( &
            tau335 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau336(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau336)

    !$omp single
    tau347 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau347)
    
    do p=1, NAO
        tau347 = tau347 + ( &
            u(p)**2 * tau334(p)&
        )
    end do
    
    !$omp end do

    allocate(tau348(NAO))
    !$omp single
    tau348 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau348(p) = tau348(p) + ( &
            tau347 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau348(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau348)

    allocate(tau474(NAO))
    !$omp single
    tau474 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau474(p) = tau474(p) + ( &
            tau334(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau334)

    allocate(tau475(NAO))
    !$omp single
    tau475 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau475(p) = tau475(p) + ( &
            tau474(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau474)

    !$omp single
    tau476 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau476)
    
    do p=1, NAO
        tau476 = tau476 + ( &
            t1(p) * tau475(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau475)

    allocate(tau477(NAO))
    !$omp single
    tau477 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau477(p) = tau477(p) + ( &
            tau476 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                4 * one(p) * tau477(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau477)

    allocate(tau280(NAO))
    !$omp single
    tau280 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau280(p) = tau280(p) + ( &
                tau34(q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau281(NAO))
    !$omp single
    tau281 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau281(p) = tau281(p) + ( &
            tau280(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau282 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau282)
    
    do p=1, NAO
        tau282 = tau282 + ( &
            tau281(p) * u(p)&
        )
    end do
    
    !$omp end do

    allocate(tau283(NAO))
    !$omp single
    tau283 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau283(p) = tau283(p) + ( &
            tau282 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau283(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau283)

    allocate(tau370(NAO))
    !$omp single
    tau370 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau370(p) = tau370(p) + ( &
            tau281(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau281)

    !$omp single
    tau371 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau371)
    
    do p=1, NAO
        tau371 = tau371 + ( &
            t1(p)**2 * tau370(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau370)

    allocate(tau372(NAO))
    !$omp single
    tau372 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau372(p) = tau372(p) + ( &
            tau371 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau372(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau372)

    allocate(tau327(NAO))
    !$omp single
    tau327 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau327(p) = tau327(p) + ( &
            t1(p) * tau280(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau328 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau328)
    
    do p=1, NAO
        tau328 = tau328 + ( &
            v(p)**2 * tau327(p)&
        )
    end do
    
    !$omp end do

    allocate(tau329(NAO))
    !$omp single
    tau329 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau329(p) = tau329(p) + ( &
            tau328 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau329(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau329)

    !$omp single
    tau341 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau341)
    
    do p=1, NAO
        tau341 = tau341 + ( &
            u(p)**2 * tau327(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau327)

    allocate(tau342(NAO))
    !$omp single
    tau342 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau342(p) = tau342(p) + ( &
            tau341 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau342(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau342)

    allocate(tau416(NAO))
    !$omp single
    tau416 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau416(p) = tau416(p) + ( &
            tau280(p) * t2(p, p)&
        )
    
    end do
    !$omp end do

    deallocate(tau280)

    allocate(tau417(NAO))
    !$omp single
    tau417 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau417(p) = tau417(p) + ( &
            tau416(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau416)

    !$omp single
    tau418 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau418)
    
    do p=1, NAO
        tau418 = tau418 + ( &
            tau417(p) * u(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau417)

    allocate(tau419(NAO))
    !$omp single
    tau419 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau419(p) = tau419(p) + ( &
            tau418 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau419(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau419)

    allocate(tau448(NAO, NAO))
    !$omp single
    tau448 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau448(p, q) = tau448(p, q) + ( &
                tau34(p) * z2(q, p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau449(NAO))
    !$omp single
    tau449 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau449(p) = tau449(p) + ( &
                t2(p, q)**2 * tau448(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau448)

    allocate(tau450(NAO))
    !$omp single
    tau450 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau450(p) = tau450(p) + ( &
            tau449(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau449)

    !$omp single
    tau451 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau451)
    
    do p=1, NAO
        tau451 = tau451 + ( &
            tau450(p) * u(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau450)

    allocate(tau452(NAO))
    !$omp single
    tau452 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau452(p) = tau452(p) + ( &
            tau451 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau452(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau452)

    allocate(tau464(NAO))
    !$omp single
    tau464 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau464(p) = tau464(p) + ( &
            t1(p)**2 * tau34(p)&
        )
    
    end do
    !$omp end do

    allocate(tau465(NAO))
    !$omp single
    tau465 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau465(p) = tau465(p) + ( &
                tau464(q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau464)

    allocate(tau466(NAO))
    !$omp single
    tau466 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau466(p) = tau466(p) + ( &
            tau465(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau465)

    allocate(tau467(NAO))
    !$omp single
    tau467 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau467(p) = tau467(p) + ( &
            tau466(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau466)

    !$omp single
    tau468 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau468)
    
    do p=1, NAO
        tau468 = tau468 + ( &
            t1(p)**2 * tau467(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau467)

    allocate(tau469(NAO))
    !$omp single
    tau469 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau469(p) = tau469(p) + ( &
            tau468 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau469(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau469)

    allocate(tau42(NAO))
    !$omp single
    tau42 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau42(p) = tau42(p) + ( &
            z1(p) * t2(p, p)&
        )
    
    end do
    !$omp end do

    allocate(tau43(NAO))
    !$omp single
    tau43 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau43(p) = tau43(p) + ( &
            tau42(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau44 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau44)
    
    do p=1, NAO
        tau44 = tau44 + ( &
            tau43(p) * u(p)&
        )
    end do
    
    !$omp end do

    allocate(tau45(NAO))
    !$omp single
    tau45 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau45(p) = tau45(p) + ( &
            tau44 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau45(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    tau480 = 0.0
    !$omp end single

    !$omp single
    
    
    tau480 = tau480 + ( &
        tau35*tau44&
    )
    
    
    !$omp end single

    allocate(tau481(NAO))
    !$omp single
    tau481 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau481(p) = tau481(p) + ( &
            tau480 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau481(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau481)

    !$omp single
    tau52 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau52)
    
    do p=1, NAO
        tau52 = tau52 + ( &
            u(p)**3 * tau43(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau43)

    allocate(tau53(NAO))
    !$omp single
    tau53 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau53(p) = tau53(p) + ( &
            tau52 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau53(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau53)

    allocate(tau54(NAO))
    !$omp single
    tau54 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau54(p) = tau54(p) + ( &
            tau42(p) * u(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau55 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau55)
    
    do p=1, NAO
        tau55 = tau55 + ( &
            v(p)**3 * tau54(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau54)

    allocate(tau56(NAO))
    !$omp single
    tau56 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau56(p) = tau56(p) + ( &
            tau55 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau56(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau56)

    !$omp single
    tau264 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau264)
    
    do p=1, NAO
        tau264 = tau264 + ( &
            v(p)**2 * tau42(p)&
        )
    end do
    
    !$omp end do

    !$omp single
    tau267 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau267)
    
    do p=1, NAO
        tau267 = tau267 + ( &
            u(p)**2 * tau42(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau42)

    allocate(tau68(NAO))
    !$omp single
    tau68 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau68(p) = tau68(p) + ( &
                u(q)**2 * t2(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau69(NAO))
    !$omp single
    tau69 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau69(p) = tau69(p) + ( &
                tau68(q) * z2(q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp single
    tau70 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau70)
    
    do p=1, NAO
        tau70 = tau70 + ( &
            u(p)**2 * tau69(p)&
        )
    end do
    
    !$omp end do

    allocate(tau71(NAO))
    !$omp single
    tau71 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau71(p) = tau71(p) + ( &
            tau70 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau71(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau71)

    allocate(tau79(NAO))
    !$omp single
    tau79 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau79(p) = tau79(p) + ( &
            v(p)**2 * tau69(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau69)

    !$omp single
    tau80 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau80)
    
    do p=1, NAO
        tau80 = tau80 + ( &
            t1(p)**2 * tau79(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau79)

    allocate(tau81(NAO))
    !$omp single
    tau81 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau81(p) = tau81(p) + ( &
            tau80 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau81(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau81)

    allocate(tau228(NAO))
    !$omp single
    tau228 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau228(p) = tau228(p) + ( &
            tau68(p) * z2(p, p)&
        )
    
    end do
    !$omp end do

    deallocate(tau68)

    !$omp single
    tau229 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau229)
    
    do p=1, NAO
        tau229 = tau229 + ( &
            u(p)**2 * tau228(p)&
        )
    end do
    
    !$omp end do

    allocate(tau230(NAO))
    !$omp single
    tau230 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau230(p) = tau230(p) + ( &
            tau229 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau230(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau230)

    allocate(tau297(NAO))
    !$omp single
    tau297 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau297(p) = tau297(p) + ( &
            v(p)**2 * tau228(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau228)

    !$omp single
    tau298 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau298)
    
    do p=1, NAO
        tau298 = tau298 + ( &
            t1(p)**2 * tau297(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau297)

    allocate(tau299(NAO))
    !$omp single
    tau299 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau299(p) = tau299(p) + ( &
            tau298 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau299(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau299)

    allocate(tau72(NAO))
    !$omp single
    tau72 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau72(p) = tau72(p) + ( &
                v(q)**2 * t2(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau73(NAO))
    !$omp single
    tau73 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau73(p) = tau73(p) + ( &
                tau72(q) * z2(q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp single
    tau74 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau74)
    
    do p=1, NAO
        tau74 = tau74 + ( &
            v(p)**2 * tau73(p)&
        )
    end do
    
    !$omp end do

    allocate(tau75(NAO))
    !$omp single
    tau75 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau75(p) = tau75(p) + ( &
            tau74 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau75(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau75)

    allocate(tau76(NAO))
    !$omp single
    tau76 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau76(p) = tau76(p) + ( &
            u(p)**2 * tau73(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau77 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau77)
    
    do p=1, NAO
        tau77 = tau77 + ( &
            t1(p)**2 * tau76(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau76)

    allocate(tau78(NAO))
    !$omp single
    tau78 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau78(p) = tau78(p) + ( &
            tau77 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau78(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau78)

    allocate(tau82(NAO))
    !$omp single
    tau82 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau82(p) = tau82(p) + ( &
            tau73(p) * t2(p, p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau83 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau83)
    
    do p=1, NAO
        tau83 = tau83 + ( &
            u(p)**2 * tau82(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau82)

    allocate(tau84(NAO))
    !$omp single
    tau84 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau84(p) = tau84(p) + ( &
            tau83 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau84(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau84)

    allocate(tau503(NAO))
    !$omp single
    tau503 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau503(p) = tau503(p) + ( &
                tau73(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau73)

    !$omp single
    tau504 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau504)
    
    do p=1, NAO
        tau504 = tau504 + ( &
            u(p)**2 * tau503(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau503)

    allocate(tau505(NAO))
    !$omp single
    tau505 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau505(p) = tau505(p) + ( &
            tau504 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau505(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau505)

    allocate(tau90(NAO))
    !$omp single
    tau90 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau90(p) = tau90(p) + ( &
            tau72(p) * z2(p, p)&
        )
    
    end do
    !$omp end do

    allocate(tau91(NAO))
    !$omp single
    tau91 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau91(p) = tau91(p) + ( &
                tau90(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp single
    tau92 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau92)
    
    do p=1, NAO
        tau92 = tau92 + ( &
            u(p)**2 * tau91(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau91)

    allocate(tau93(NAO))
    !$omp single
    tau93 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau93(p) = tau93(p) + ( &
            tau92 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau93(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau93)

    !$omp single
    tau236 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau236)
    
    do p=1, NAO
        tau236 = tau236 + ( &
            v(p)**2 * tau90(p)&
        )
    end do
    
    !$omp end do

    allocate(tau237(NAO))
    !$omp single
    tau237 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau237(p) = tau237(p) + ( &
            tau236 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau237(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau237)

    allocate(tau289(NAO))
    !$omp single
    tau289 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau289(p) = tau289(p) + ( &
            u(p)**2 * tau90(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau290 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau290)
    
    do p=1, NAO
        tau290 = tau290 + ( &
            t1(p)**2 * tau289(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau289)

    allocate(tau291(NAO))
    !$omp single
    tau291 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau291(p) = tau291(p) + ( &
            tau290 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau291(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau291)

    allocate(tau310(NAO))
    !$omp single
    tau310 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau310(p) = tau310(p) + ( &
            tau90(p) * t2(p, p)&
        )
    
    end do
    !$omp end do

    deallocate(tau90)

    !$omp single
    tau311 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau311)
    
    do p=1, NAO
        tau311 = tau311 + ( &
            u(p)**2 * tau310(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau310)

    allocate(tau312(NAO))
    !$omp single
    tau312 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau312(p) = tau312(p) + ( &
            tau311 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                3 * one(p) * tau312(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau312)

    !$omp single
    tau186 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau186)
    
    do p=1, NAO
        tau186 = tau186 + ( &
            u(p)**2 * tau72(p)&
        )
    end do
    
    !$omp end do

    allocate(tau187(NAO))
    !$omp single
    tau187 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau187(p) = tau187(p) + ( &
            tau186 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau187(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau187)

    allocate(tau337(NAO))
    !$omp single
    tau337 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau337(p) = tau337(p) + ( &
            tau72(p) * z1(p)&
        )
    
    end do
    !$omp end do

    allocate(tau338(NAO))
    !$omp single
    tau338 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau338(p) = tau338(p) + ( &
            t1(p) * tau337(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau337)

    !$omp single
    tau339 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau339)
    
    do p=1, NAO
        tau339 = tau339 + ( &
            u(p)**2 * tau338(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau338)

    allocate(tau340(NAO))
    !$omp single
    tau340 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau340(p) = tau340(p) + ( &
            tau339 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau340(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau340)

    !$omp single
    tau85 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau85)
    
    do p=1, NAO
        tau85 = tau85 + ( &
            u(p) * v(p)&
        )
    end do
    
    !$omp end do

    !$omp single
    tau216 = 0.0
    !$omp end single

    !$omp single
    
    
    tau216 = tau216 + ( &
        tau85**2&
    )
    
    
    !$omp end single

    allocate(tau217(NAO))
    !$omp single
    tau217 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau217(p) = tau217(p) + ( &
            tau216 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau217(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau217)

    !$omp single
    tau275 = 0.0
    !$omp end single

    !$omp single
    
    
    tau275 = tau275 + ( &
        tau264*tau85&
    )
    
    
    !$omp end single

    allocate(tau276(NAO))
    !$omp single
    tau276 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau276(p) = tau276(p) + ( &
            tau275 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau276(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau276)

    !$omp single
    tau308 = 0.0
    !$omp end single

    !$omp single
    
    
    tau308 = tau308 + ( &
        tau267*tau85&
    )
    
    
    !$omp end single

    allocate(tau309(NAO))
    !$omp single
    tau309 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau309(p) = tau309(p) + ( &
            tau308 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau309(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau309)

    !$omp single
    tau368 = 0.0
    !$omp end single

    !$omp single
    
    
    tau368 = tau368 + ( &
        tau367*tau85&
    )
    
    
    !$omp end single

    allocate(tau369(NAO))
    !$omp single
    tau369 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau369(p) = tau369(p) + ( &
            tau368 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                4 * one(p) * tau369(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau369)

    !$omp single
    tau400 = 0.0
    !$omp end single

    !$omp single
    
    
    tau400 = tau400 + ( &
        tau393*tau85&
    )
    
    
    !$omp end single

    allocate(tau401(NAO))
    !$omp single
    tau401 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau401(p) = tau401(p) + ( &
            tau400 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau401(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau401)

    !$omp single
    tau430 = 0.0
    !$omp end single

    !$omp single
    
    
    tau430 = tau430 + ( &
        tau381*tau85&
    )
    
    
    !$omp end single

    allocate(tau431(NAO))
    !$omp single
    tau431 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau431(p) = tau431(p) + ( &
            tau430 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau431(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau431)

    !$omp single
    tau459 = 0.0
    !$omp end single

    !$omp single
    
    
    tau459 = tau459 + ( &
        tau402*tau85&
    )
    
    
    !$omp end single

    allocate(tau460(NAO))
    !$omp single
    tau460 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau460(p) = tau460(p) + ( &
            tau459 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                4 * one(p) * tau460(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau460)

    allocate(tau86(NAO))
    !$omp single
    tau86 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau86(p) = tau86(p) + ( &
                z1(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp single
    tau87 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau87)
    
    do p=1, NAO
        tau87 = tau87 + ( &
            u(p)**2 * tau86(p)&
        )
    end do
    
    !$omp end do

    !$omp single
    tau88 = 0.0
    !$omp end single

    !$omp single
    
    
    tau88 = tau88 + ( &
        tau85*tau87&
    )
    
    
    !$omp end single

    allocate(tau89(NAO))
    !$omp single
    tau89 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau89(p) = tau89(p) + ( &
            tau88 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau89(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau89)

    !$omp single
    tau100 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau100)
    
    do p=1, NAO
        tau100 = tau100 + ( &
            v(p)**2 * tau86(p)&
        )
    end do
    
    !$omp end do

    !$omp single
    tau106 = 0.0
    !$omp end single

    !$omp single
    
    
    tau106 = tau106 + ( &
        tau100*tau85&
    )
    
    
    !$omp end single

    allocate(tau107(NAO))
    !$omp single
    tau107 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau107(p) = tau107(p) + ( &
            tau106 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau107(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau107)

    allocate(tau174(NAO))
    !$omp single
    tau174 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau174(p) = tau174(p) + ( &
            tau86(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau175 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau175)
    
    do p=1, NAO
        tau175 = tau175 + ( &
            tau174(p) * u(p)&
        )
    end do
    
    !$omp end do

    !$omp single
    tau176 = 0.0
    !$omp end single

    !$omp single
    
    
    tau176 = tau176 + ( &
        tau175*tau35&
    )
    
    
    !$omp end single

    allocate(tau177(NAO))
    !$omp single
    tau177 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau177(p) = tau177(p) + ( &
            tau176 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau177(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau177)

    allocate(tau242(NAO))
    !$omp single
    tau242 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau242(p) = tau242(p) + ( &
            tau175 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau242(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    tau203 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau203)
    
    do p=1, NAO
        tau203 = tau203 + ( &
            u(p)**3 * tau174(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau174)

    allocate(tau204(NAO))
    !$omp single
    tau204 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau204(p) = tau204(p) + ( &
            tau203 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau204(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau204)

    allocate(tau213(NAO))
    !$omp single
    tau213 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau213(p) = tau213(p) + ( &
            tau86(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau86)

    !$omp single
    tau214 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau214)
    
    do p=1, NAO
        tau214 = tau214 + ( &
            v(p)**3 * tau213(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau213)

    allocate(tau215(NAO))
    !$omp single
    tau215 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau215(p) = tau215(p) + ( &
            tau214 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau215(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau215)

    allocate(tau94(NAO))
    !$omp single
    tau94 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau94(p) = tau94(p) + ( &
            v(p)**2 * t2(p, p)&
        )
    
    end do
    !$omp end do

    allocate(tau95(NAO))
    !$omp single
    tau95 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau95(p) = tau95(p) + ( &
                tau94(q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau94)

    allocate(tau96(NAO))
    !$omp single
    tau96 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau96(p) = tau96(p) + ( &
                tau95(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    !$omp single
    tau97 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau97)
    
    do p=1, NAO
        tau97 = tau97 + ( &
            u(p)**2 * tau96(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau96)

    allocate(tau98(NAO))
    !$omp single
    tau98 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau98(p) = tau98(p) + ( &
            tau97 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau98(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau98)

    allocate(tau292(NAO))
    !$omp single
    tau292 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau292(p) = tau292(p) + ( &
            u(p)**2 * tau95(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau293 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau293)
    
    do p=1, NAO
        tau293 = tau293 + ( &
            t1(p)**2 * tau292(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau292)

    allocate(tau294(NAO))
    !$omp single
    tau294 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau294(p) = tau294(p) + ( &
            tau293 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau294(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau294)

    allocate(tau305(NAO))
    !$omp single
    tau305 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau305(p) = tau305(p) + ( &
            tau95(p) * t2(p, p)&
        )
    
    end do
    !$omp end do

    deallocate(tau95)

    !$omp single
    tau306 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau306)
    
    do p=1, NAO
        tau306 = tau306 + ( &
            u(p)**2 * tau305(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau305)

    allocate(tau307(NAO))
    !$omp single
    tau307 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau307(p) = tau307(p) + ( &
            tau306 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau307(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau307)

    !$omp single
    tau99 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau99)
    
    do p=1, NAO
        tau99 = tau99 + ( &
            u(p)**2 * t1(p)&
        )
    end do
    
    !$omp end do

    !$omp single
    tau101 = 0.0
    !$omp end single

    !$omp single
    
    
    tau101 = tau101 + ( &
        tau100*tau99&
    )
    
    
    !$omp end single

    allocate(tau102(NAO))
    !$omp single
    tau102 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau102(p) = tau102(p) + ( &
            tau101 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau102(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau102)

    !$omp single
    tau201 = 0.0
    !$omp end single

    !$omp single
    
    
    tau201 = tau201 + ( &
        tau85*tau99&
    )
    
    
    !$omp end single

    allocate(tau202(NAO))
    !$omp single
    tau202 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau202(p) = tau202(p) + ( &
            tau201 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau202(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau202)

    !$omp single
    tau265 = 0.0
    !$omp end single

    !$omp single
    
    
    tau265 = tau265 + ( &
        tau264*tau99&
    )
    
    
    !$omp end single

    allocate(tau266(NAO))
    !$omp single
    tau266 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau266(p) = tau266(p) + ( &
            tau265 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau266(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau266)

    !$omp single
    tau394 = 0.0
    !$omp end single

    !$omp single
    
    
    tau394 = tau394 + ( &
        tau393*tau99&
    )
    
    
    !$omp end single

    allocate(tau395(NAO))
    !$omp single
    tau395 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau395(p) = tau395(p) + ( &
            tau394 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau395(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau395)

    !$omp single
    tau396 = 0.0
    !$omp end single

    !$omp single
    
    
    tau396 = tau396 + ( &
        tau367*tau99&
    )
    
    
    !$omp end single

    allocate(tau397(NAO))
    !$omp single
    tau397 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau397(p) = tau397(p) + ( &
            tau396 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau397(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau397)

    !$omp single
    tau432 = 0.0
    !$omp end single

    !$omp single
    
    
    tau432 = tau432 + ( &
        tau402*tau99&
    )
    
    
    !$omp end single

    allocate(tau433(NAO))
    !$omp single
    tau433 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau433(p) = tau433(p) + ( &
            tau432 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau433(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau433)

    !$omp single
    tau103 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau103)
    
    do p=1, NAO
        tau103 = tau103 + ( &
            v(p)**2 * t1(p)&
        )
    end do
    
    !$omp end do

    !$omp single
    tau104 = 0.0
    !$omp end single

    !$omp single
    
    
    tau104 = tau104 + ( &
        tau103*tau87&
    )
    
    
    !$omp end single

    allocate(tau105(NAO))
    !$omp single
    tau105 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau105(p) = tau105(p) + ( &
            tau104 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau105(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau105)

    !$omp single
    tau231 = 0.0
    !$omp end single

    !$omp single
    
    
    tau231 = tau231 + ( &
        tau103*tau99&
    )
    
    
    !$omp end single

    allocate(tau232(NAO))
    !$omp single
    tau232 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau232(p) = tau232(p) + ( &
            tau231 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau232(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau232)

    !$omp single
    tau238 = 0.0
    !$omp end single

    !$omp single
    
    
    tau238 = tau238 + ( &
        tau103*tau85&
    )
    
    
    !$omp end single

    allocate(tau239(NAO))
    !$omp single
    tau239 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau239(p) = tau239(p) + ( &
            tau238 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau239(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau239)

    !$omp single
    tau268 = 0.0
    !$omp end single

    !$omp single
    
    
    tau268 = tau268 + ( &
        tau103*tau267&
    )
    
    
    !$omp end single

    allocate(tau269(NAO))
    !$omp single
    tau269 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau269(p) = tau269(p) + ( &
            tau268 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau269(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau269)

    !$omp single
    tau382 = 0.0
    !$omp end single

    !$omp single
    
    
    tau382 = tau382 + ( &
        tau103*tau381&
    )
    
    
    !$omp end single

    allocate(tau383(NAO))
    !$omp single
    tau383 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau383(p) = tau383(p) + ( &
            tau382 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau383(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau383)

    !$omp single
    tau403 = 0.0
    !$omp end single

    !$omp single
    
    
    tau403 = tau403 + ( &
        tau103*tau402&
    )
    
    
    !$omp end single

    allocate(tau404(NAO))
    !$omp single
    tau404 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau404(p) = tau404(p) + ( &
            tau403 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau404(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau404)

    !$omp single
    tau444 = 0.0
    !$omp end single

    !$omp single
    
    
    tau444 = tau444 + ( &
        tau103*tau367&
    )
    
    
    !$omp end single

    allocate(tau445(NAO))
    !$omp single
    tau445 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau445(p) = tau445(p) + ( &
            tau444 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau445(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau445)

    allocate(tau108(NAO))
    !$omp single
    tau108 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau108(p) = tau108(p) + ( &
                t2(q, p) * z2(q, p)&
            )
        end do
    end do
    !$omp end do

    allocate(tau109(NAO))
    !$omp single
    tau109 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau109(p) = tau109(p) + ( &
            tau108(p) * tau72(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau72)

    !$omp single
    tau110 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau110)
    
    do p=1, NAO
        tau110 = tau110 + ( &
            u(p)**2 * tau109(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau109)

    allocate(tau111(NAO))
    !$omp single
    tau111 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau111(p) = tau111(p) + ( &
            tau110 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau111(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau111)

    allocate(tau112(NAO))
    !$omp single
    tau112 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau112(p) = tau112(p) + ( &
            v(p)**2 * tau108(p)&
        )
    
    end do
    !$omp end do

    allocate(tau113(NAO))
    !$omp single
    tau113 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau113(p) = tau113(p) + ( &
                tau112(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau112)

    !$omp single
    tau114 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau114)
    
    do p=1, NAO
        tau114 = tau114 + ( &
            u(p)**2 * tau113(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau113)

    allocate(tau115(NAO))
    !$omp single
    tau115 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau115(p) = tau115(p) + ( &
            tau114 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau115(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau115)

    allocate(tau116(NAO))
    !$omp single
    tau116 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau116(p) = tau116(p) + ( &
            tau108(p) * v(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau117 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau117)
    
    do p=1, NAO
        tau117 = tau117 + ( &
            tau116(p) * u(p)&
        )
    end do
    
    !$omp end do

    !$omp single
    tau118 = 0.0
    !$omp end single

    !$omp single
    
    
    tau118 = tau118 + ( &
        tau117*tau85&
    )
    
    
    !$omp end single

    allocate(tau119(NAO))
    !$omp single
    tau119 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau119(p) = tau119(p) + ( &
            tau118 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                4 * one(p) * tau119(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau119)

    !$omp single
    tau124 = 0.0
    !$omp end single

    !$omp single
    
    
    tau124 = tau124 + ( &
        tau117*tau99&
    )
    
    
    !$omp end single

    allocate(tau125(NAO))
    !$omp single
    tau125 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau125(p) = tau125(p) + ( &
            tau124 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau125(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau125)

    !$omp single
    tau151 = 0.0
    !$omp end single

    !$omp single
    
    
    tau151 = tau151 + ( &
        tau103*tau117&
    )
    
    
    !$omp end single

    allocate(tau152(NAO))
    !$omp single
    tau152 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau152(p) = tau152(p) + ( &
            tau151 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau152(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau152)

    allocate(tau178(NAO))
    !$omp single
    tau178 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau178(p) = tau178(p) + ( &
            tau116(p) * u(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau179 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau179)
    
    do p=1, NAO
        tau179 = tau179 + ( &
            t1(p) * tau178(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau178)

    !$omp single
    tau180 = 0.0
    !$omp end single

    !$omp single
    
    
    tau180 = tau180 + ( &
        tau179*tau35&
    )
    
    
    !$omp end single

    allocate(tau181(NAO))
    !$omp single
    tau181 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau181(p) = tau181(p) + ( &
            tau180 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                4 * one(p) * tau181(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau181)

    allocate(tau353(NAO))
    !$omp single
    tau353 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau353(p) = tau353(p) + ( &
            tau179 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau353(q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau319(NAO))
    !$omp single
    tau319 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau319(p) = tau319(p) + ( &
            t1(p) * tau116(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau116)

    !$omp single
    tau320 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau320)
    
    do p=1, NAO
        tau320 = tau320 + ( &
            u(p)**3 * tau319(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau319)

    allocate(tau321(NAO))
    !$omp single
    tau321 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau321(p) = tau321(p) + ( &
            tau320 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau321(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau321)

    allocate(tau120(NAO))
    !$omp single
    tau120 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau120(p) = tau120(p) + ( &
            t1(p) * tau108(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau121 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau121)
    
    do p=1, NAO
        tau121 = tau121 + ( &
            u(p)**2 * tau120(p)&
        )
    end do
    
    !$omp end do

    !$omp single
    tau122 = 0.0
    !$omp end single

    !$omp single
    
    
    tau122 = tau122 + ( &
        tau121*tau85&
    )
    
    
    !$omp end single

    allocate(tau123(NAO))
    !$omp single
    tau123 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau123(p) = tau123(p) + ( &
            tau122 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau123(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau123)

    !$omp single
    tau139 = 0.0
    !$omp end single

    !$omp single
    
    
    tau139 = tau139 + ( &
        tau103*tau121&
    )
    
    
    !$omp end single

    allocate(tau140(NAO))
    !$omp single
    tau140 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau140(p) = tau140(p) + ( &
            tau139 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau140(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau140)

    !$omp single
    tau143 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau143)
    
    do p=1, NAO
        tau143 = tau143 + ( &
            v(p)**2 * tau120(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau120)

    !$omp single
    tau144 = 0.0
    !$omp end single

    !$omp single
    
    
    tau144 = tau144 + ( &
        tau143*tau99&
    )
    
    
    !$omp end single

    allocate(tau145(NAO))
    !$omp single
    tau145 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau145(p) = tau145(p) + ( &
            tau144 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau145(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau145)

    !$omp single
    tau149 = 0.0
    !$omp end single

    !$omp single
    
    
    tau149 = tau149 + ( &
        tau143*tau85&
    )
    
    
    !$omp end single

    allocate(tau150(NAO))
    !$omp single
    tau150 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau150(p) = tau150(p) + ( &
            tau149 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau150(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau150)

    !$omp single
    tau136 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau136)
    
    do p=1, NAO
        tau136 = tau136 + ( &
            v(p)**2 * tau108(p)&
        )
    end do
    
    !$omp end do

    !$omp single
    tau137 = 0.0
    !$omp end single

    !$omp single
    
    
    tau137 = tau137 + ( &
        tau136*tau35&
    )
    
    
    !$omp end single

    allocate(tau138(NAO))
    !$omp single
    tau138 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau138(p) = tau138(p) + ( &
            tau137 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau138(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau138)

    allocate(tau185(NAO))
    !$omp single
    tau185 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau185(p) = tau185(p) + ( &
            tau136 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau185(q)&
            )
    
        end do
    end do
    !$omp end do

    !$omp single
    tau146 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau146)
    
    do p=1, NAO
        tau146 = tau146 + ( &
            u(p)**2 * tau108(p)&
        )
    end do
    
    !$omp end do

    !$omp single
    tau147 = 0.0
    !$omp end single

    !$omp single
    
    
    tau147 = tau147 + ( &
        tau146*tau35&
    )
    
    
    !$omp end single

    allocate(tau148(NAO))
    !$omp single
    tau148 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau148(p) = tau148(p) + ( &
            tau147 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau148(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau148)

    allocate(tau191(NAO))
    !$omp single
    tau191 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau191(p) = tau191(p) + ( &
            tau146 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau191(q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau163(NAO))
    !$omp single
    tau163 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau163(p) = tau163(p) + ( &
            tau108(p) * tau131(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau131)

    allocate(tau164(NAO))
    !$omp single
    tau164 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau164(p) = tau164(p) + ( &
            tau163(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau163)

    !$omp single
    tau165 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau165)
    
    do p=1, NAO
        tau165 = tau165 + ( &
            tau164(p) * u(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau164)

    allocate(tau166(NAO))
    !$omp single
    tau166 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau166(p) = tau166(p) + ( &
            tau165 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                4 * one(p) * tau166(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau166)

    !$omp single
    tau183 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau183)
    
    do p=1, NAO
        tau183 = tau183 + ( &
            u(p)**4 * tau108(p)&
        )
    end do
    
    !$omp end do

    allocate(tau184(NAO))
    !$omp single
    tau184 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau184(p) = tau184(p) + ( &
            tau183 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau184(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau184)

    !$omp single
    tau192 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau192)
    
    do p=1, NAO
        tau192 = tau192 + ( &
            v(p)**4 * tau108(p)&
        )
    end do
    
    !$omp end do

    allocate(tau193(NAO))
    !$omp single
    tau193 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau193(p) = tau193(p) + ( &
            tau192 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau193(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau193)

    allocate(tau330(NAO))
    !$omp single
    tau330 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau330(p) = tau330(p) + ( &
            tau108(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau108)

    allocate(tau331(NAO))
    !$omp single
    tau331 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau331(p) = tau331(p) + ( &
            t1(p) * tau330(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau330)

    !$omp single
    tau332 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau332)
    
    do p=1, NAO
        tau332 = tau332 + ( &
            v(p)**3 * tau331(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau331)

    allocate(tau333(NAO))
    !$omp single
    tau333 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau333(p) = tau333(p) + ( &
            tau332 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau333(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau333)

    allocate(tau182(NAO))
    !$omp single
    tau182 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau182(p) = tau182(p) + ( &
            v(p)**2 * one(p)&
        )
    
    end do
    !$omp end do

    allocate(tau510(NAO, NAO))
    !$omp single
    tau510 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau510(p, q) = tau510(p, q) + ( &
                one(q) * tau182(p)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau524(NAO, NAO))
    !$omp single
    tau524 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau524(p, q) = tau524(p, q) - ( &
                tau510(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau510)

    allocate(tau511(NAO, NAO))
    !$omp single
    tau511 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau511(p, q) = tau511(p, q) + ( &
                tau182(p) * tau36(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau36)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau524(p, q) = tau524(p, q) + ( &
                2 * tau511(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau511)

    allocate(tau512(NAO, NAO))
    !$omp single
    tau512 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau512(p, q) = tau512(p, q) + ( &
                tau182(p) * tau38(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau38)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau524(p, q) = tau524(p, q) + ( &
                2 * tau512(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau512)

    allocate(tau513(NAO, NAO))
    !$omp single
    tau513 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau513(p, q) = tau513(p, q) + ( &
                tau182(p) * tau27(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau27)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau524(p, q) = tau524(p, q) + ( &
                2 * tau513(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau513)

    allocate(tau514(NAO, NAO))
    !$omp single
    tau514 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau514(p, q) = tau514(p, q) + ( &
                tau182(p) * tau31(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau31)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau524(p, q) = tau524(p, q) + ( &
                2 * tau514(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau514)

    allocate(tau515(NAO, NAO))
    !$omp single
    tau515 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau515(p, q) = tau515(p, q) + ( &
                tau17(q) * tau182(p)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau17)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau524(p, q) = tau524(p, q) - ( &
                2 * tau515(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau515)

    allocate(tau516(NAO, NAO))
    !$omp single
    tau516 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau516(p, q) = tau516(p, q) + ( &
                tau182(p) * tau6(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau6)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau524(p, q) = tau524(p, q) - ( &
                2 * tau516(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau516)

    allocate(tau517(NAO, NAO))
    !$omp single
    tau517 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau517(p, q) = tau517(p, q) + ( &
                tau182(p) * tau45(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau45)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau524(p, q) = tau524(p, q) - ( &
                2 * tau517(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau517)

    allocate(tau518(NAO, NAO))
    !$omp single
    tau518 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau518(p, q) = tau518(p, q) + ( &
                tau182(p) * tau60(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau60)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau524(p, q) = tau524(p, q) + ( &
                4 * tau518(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau518)

    allocate(tau519(NAO, NAO))
    !$omp single
    tau519 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau519(p, q) = tau519(p, q) + ( &
                tau182(p) * tau41(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau41)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau524(p, q) = tau524(p, q) - ( &
                2 * tau519(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau519)

    allocate(tau520(NAO, NAO))
    !$omp single
    tau520 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau520(p, q) = tau520(p, q) + ( &
                tau182(p) * tau191(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau191)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau524(p, q) = tau524(p, q) + ( &
                2 * tau520(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau520)

    allocate(tau521(NAO, NAO))
    !$omp single
    tau521 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau521(p, q) = tau521(p, q) + ( &
                tau182(p) * tau242(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau242)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau524(p, q) = tau524(p, q) + ( &
                2 * tau521(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau521)

    allocate(tau522(NAO, NAO))
    !$omp single
    tau522 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau522(p, q) = tau522(p, q) + ( &
                tau182(p) * tau185(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau185)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau524(p, q) = tau524(p, q) - ( &
                2 * tau522(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau522)

    allocate(tau523(NAO, NAO))
    !$omp single
    tau523 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau523(p, q) = tau523(p, q) + ( &
                tau182(p) * tau353(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau353)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau524(p, q) = tau524(p, q) - ( &
                4 * tau523(p, q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau523)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                tau524(p, q) / 2&
            )
    
        end do
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                tau524(q, p) / 2&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau524)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                tau182(p) * tau182(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau182)

    allocate(tau188(NAO))
    !$omp single
    tau188 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau188(p) = tau188(p) + ( &
                v(q)**2 * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp single
    tau189 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau189)
    
    do p=1, NAO
        tau189 = tau189 + ( &
            u(p)**2 * tau188(p)&
        )
    end do
    
    !$omp end do

    allocate(tau190(NAO))
    !$omp single
    tau190 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau190(p) = tau190(p) + ( &
            tau189 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau190(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau190)

    allocate(tau222(NAO))
    !$omp single
    tau222 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau222(p) = tau222(p) + ( &
            v(p)**2 * tau188(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau223 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau223)
    
    do p=1, NAO
        tau223 = tau223 + ( &
            t1(p)**2 * tau222(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau222)

    allocate(tau224(NAO))
    !$omp single
    tau224 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau224(p) = tau224(p) + ( &
            tau223 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau224(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau224)

    allocate(tau233(NAO))
    !$omp single
    tau233 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau233(p) = tau233(p) + ( &
            tau188(p) * t2(p, p)&
        )
    
    end do
    !$omp end do

    deallocate(tau188)

    !$omp single
    tau234 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau234)
    
    do p=1, NAO
        tau234 = tau234 + ( &
            v(p)**2 * tau233(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau233)

    allocate(tau235(NAO))
    !$omp single
    tau235 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau235(p) = tau235(p) + ( &
            tau234 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau235(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau235)

    !$omp single
    tau194 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau194)
    
    do p=1, NAO
        tau194 = tau194 + ( &
            u(p)**2 * z1(p)&
        )
    end do
    
    !$omp end do

    !$omp single
    tau195 = 0.0
    !$omp end single

    !$omp single
    
    
    tau195 = tau195 + ( &
        tau194*tau99&
    )
    
    
    !$omp end single

    allocate(tau196(NAO))
    !$omp single
    tau196 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau196(p) = tau196(p) + ( &
            tau195 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau196(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau196)

    !$omp single
    tau205 = 0.0
    !$omp end single

    !$omp single
    
    
    tau205 = tau205 + ( &
        tau194*tau85&
    )
    
    
    !$omp end single

    allocate(tau206(NAO))
    !$omp single
    tau206 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau206(p) = tau206(p) + ( &
            tau205 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau206(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau206)

    allocate(tau197(NAO, NAO))
    !$omp single
    tau197 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau197(p, q) = tau197(p, q) + ( &
                t2(p, q) * z2(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau198(NAO))
    !$omp single
    tau198 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau198(p) = tau198(p) + ( &
                u(q)**2 * tau197(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp single
    tau199 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau199)
    
    do p=1, NAO
        tau199 = tau199 + ( &
            u(p)**2 * tau198(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau198)

    allocate(tau200(NAO))
    !$omp single
    tau200 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau200(p) = tau200(p) + ( &
            tau199 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau200(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau200)

    allocate(tau210(NAO))
    !$omp single
    tau210 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau210(p) = tau210(p) + ( &
                v(q)**2 * tau197(p, q)&
            )
        end do
    end do
    !$omp end do

    !$omp single
    tau211 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau211)
    
    do p=1, NAO
        tau211 = tau211 + ( &
            v(p)**2 * tau210(p)&
        )
    end do
    
    !$omp end do

    allocate(tau212(NAO))
    !$omp single
    tau212 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau212(p) = tau212(p) + ( &
            tau211 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau212(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau212)

    !$omp single
    tau247 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau247)
    
    do p=1, NAO
        tau247 = tau247 + ( &
            u(p)**2 * tau210(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau210)

    allocate(tau248(NAO))
    !$omp single
    tau248 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau248(p) = tau248(p) + ( &
            tau247 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau248(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau248)

    allocate(tau360(NAO))
    !$omp single
    tau360 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau360(p) = tau360(p) + ( &
                tau126(q) * tau197(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau126)

    !$omp single
    tau361 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau361)
    
    do p=1, NAO
        tau361 = tau361 + ( &
            u(p)**2 * tau360(p)&
        )
    end do
    
    !$omp end do

    allocate(tau362(NAO))
    !$omp single
    tau362 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau362(p) = tau362(p) + ( &
            tau361 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                4 * one(p) * tau362(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau362)

    !$omp single
    tau457 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau457)
    
    do p=1, NAO
        tau457 = tau457 + ( &
            v(p)**2 * tau360(p)&
        )
    end do
    
    !$omp end do

    allocate(tau458(NAO))
    !$omp single
    tau458 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau458(p) = tau458(p) + ( &
            tau457 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                4 * one(p) * tau458(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau458)

    allocate(tau499(NAO))
    !$omp single
    tau499 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau499(p) = tau499(p) + ( &
            tau360(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau360)

    allocate(tau500(NAO))
    !$omp single
    tau500 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau500(p) = tau500(p) + ( &
            tau499(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau499)

    !$omp single
    tau501 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau501)
    
    do p=1, NAO
        tau501 = tau501 + ( &
            t1(p) * tau500(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau500)

    allocate(tau502(NAO))
    !$omp single
    tau502 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau502(p) = tau502(p) + ( &
            tau501 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                4 * one(p) * tau502(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau502)

    allocate(tau363(NAO))
    !$omp single
    tau363 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau363(p) = tau363(p) + ( &
                tau34(q) * tau197(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau34)

    allocate(tau364(NAO))
    !$omp single
    tau364 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau364(p) = tau364(p) + ( &
            t1(p) * tau363(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau365 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau365)
    
    do p=1, NAO
        tau365 = tau365 + ( &
            v(p)**2 * tau364(p)&
        )
    end do
    
    !$omp end do

    allocate(tau366(NAO))
    !$omp single
    tau366 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau366(p) = tau366(p) + ( &
            tau365 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                4 * one(p) * tau366(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau366)

    !$omp single
    tau455 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau455)
    
    do p=1, NAO
        tau455 = tau455 + ( &
            u(p)**2 * tau364(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau364)

    allocate(tau456(NAO))
    !$omp single
    tau456 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau456(p) = tau456(p) + ( &
            tau455 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                4 * one(p) * tau456(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau456)

    allocate(tau461(NAO))
    !$omp single
    tau461 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau461(p) = tau461(p) + ( &
            tau363(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau363)

    !$omp single
    tau462 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau462)
    
    do p=1, NAO
        tau462 = tau462 + ( &
            tau461(p) * u(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau461)

    allocate(tau463(NAO))
    !$omp single
    tau463 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau463(p) = tau463(p) + ( &
            tau462 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                4 * one(p) * tau463(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau463)

    !$omp single
    tau207 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau207)
    
    do p=1, NAO
        tau207 = tau207 + ( &
            v(p)**2 * z1(p)&
        )
    end do
    
    !$omp end do

    !$omp single
    tau208 = 0.0
    !$omp end single

    !$omp single
    
    
    tau208 = tau208 + ( &
        tau103*tau207&
    )
    
    
    !$omp end single

    allocate(tau209(NAO))
    !$omp single
    tau209 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau209(p) = tau209(p) + ( &
            tau208 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau209(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau209)

    !$omp single
    tau240 = 0.0
    !$omp end single

    !$omp single
    
    
    tau240 = tau240 + ( &
        tau207*tau85&
    )
    
    
    !$omp end single

    allocate(tau241(NAO))
    !$omp single
    tau241 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau241(p) = tau241(p) + ( &
            tau240 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau241(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau241)

    allocate(tau218(NAO))
    !$omp single
    tau218 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau218(p) = tau218(p) + ( &
                u(q)**2 * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau219(NAO))
    !$omp single
    tau219 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau219(p) = tau219(p) + ( &
            u(p)**2 * tau218(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau220 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau220)
    
    do p=1, NAO
        tau220 = tau220 + ( &
            t1(p)**2 * tau219(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau219)

    allocate(tau221(NAO))
    !$omp single
    tau221 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau221(p) = tau221(p) + ( &
            tau220 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau221(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau221)

    allocate(tau225(NAO))
    !$omp single
    tau225 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau225(p) = tau225(p) + ( &
            tau218(p) * t2(p, p)&
        )
    
    end do
    !$omp end do

    deallocate(tau218)

    !$omp single
    tau226 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau226)
    
    do p=1, NAO
        tau226 = tau226 + ( &
            u(p)**2 * tau225(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau225)

    allocate(tau227(NAO))
    !$omp single
    tau227 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau227(p) = tau227(p) + ( &
            tau226 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau227(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau227)

    allocate(tau243(NAO, NAO))
    !$omp single
    tau243 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            tau243(p, q) = tau243(p, q) + ( &
                t2(p, q)**2 * z2(p, q)&
            )
    
        end do
    end do
    !$omp end do

    allocate(tau244(NAO))
    !$omp single
    tau244 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau244(p) = tau244(p) + ( &
                v(q)**2 * tau243(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau243)

    !$omp single
    tau245 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau245)
    
    do p=1, NAO
        tau245 = tau245 + ( &
            u(p)**2 * tau244(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau244)

    allocate(tau246(NAO))
    !$omp single
    tau246 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau246(p) = tau246(p) + ( &
            tau245 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau246(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau246)

    allocate(tau249(NAO))
    !$omp single
    tau249 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau249(p) = tau249(p) + ( &
            v(p)**2 * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau250 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau250)
    
    do p=1, NAO
        tau250 = tau250 + ( &
            t1(p)**2 * tau249(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau249)

    !$omp single
    tau251 = 0.0
    !$omp end single

    !$omp single
    
    
    tau251 = tau251 + ( &
        tau250*tau85&
    )
    
    
    !$omp end single

    allocate(tau252(NAO))
    !$omp single
    tau252 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau252(p) = tau252(p) + ( &
            tau251 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau252(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau252)

    !$omp single
    tau253 = 0.0
    !$omp end single

    !$omp single
    
    
    tau253 = tau253 + ( &
        tau250*tau99&
    )
    
    
    !$omp end single

    allocate(tau254(NAO))
    !$omp single
    tau254 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau254(p) = tau254(p) + ( &
            tau253 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau254(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau254)

    allocate(tau255(NAO))
    !$omp single
    tau255 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau255(p) = tau255(p) + ( &
            u(p)**2 * z1(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau256 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau256)
    
    do p=1, NAO
        tau256 = tau256 + ( &
            t1(p)**2 * tau255(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau255)

    !$omp single
    tau257 = 0.0
    !$omp end single

    !$omp single
    
    
    tau257 = tau257 + ( &
        tau103*tau256&
    )
    
    
    !$omp end single

    allocate(tau258(NAO))
    !$omp single
    tau258 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau258(p) = tau258(p) + ( &
            tau257 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau258(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau258)

    !$omp single
    tau295 = 0.0
    !$omp end single

    !$omp single
    
    
    tau295 = tau295 + ( &
        tau256*tau85&
    )
    
    
    !$omp end single

    allocate(tau296(NAO))
    !$omp single
    tau296 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau296(p) = tau296(p) + ( &
            tau295 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau296(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau296)

    allocate(tau259(NAO))
    !$omp single
    tau259 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau259(p) = tau259(p) + ( &
            u(p)**2 * t1(p)&
        )
    
    end do
    !$omp end do

    allocate(tau260(NAO))
    !$omp single
    tau260 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau260(p) = tau260(p) + ( &
                tau259(q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau259)

    allocate(tau261(NAO))
    !$omp single
    tau261 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau261(p) = tau261(p) + ( &
            t1(p) * tau260(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau262 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau262)
    
    do p=1, NAO
        tau262 = tau262 + ( &
            u(p)**2 * tau261(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau261)

    allocate(tau263(NAO))
    !$omp single
    tau263 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau263(p) = tau263(p) + ( &
            tau262 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau263(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau263)

    allocate(tau373(NAO))
    !$omp single
    tau373 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau373(p) = tau373(p) + ( &
            tau260(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau260)

    allocate(tau374(NAO))
    !$omp single
    tau374 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau374(p) = tau374(p) + ( &
            tau373(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau373)

    !$omp single
    tau375 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau375)
    
    do p=1, NAO
        tau375 = tau375 + ( &
            t1(p)**2 * tau374(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau374)

    allocate(tau376(NAO))
    !$omp single
    tau376 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau376(p) = tau376(p) + ( &
            tau375 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau376(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau376)

    allocate(tau270(NAO))
    !$omp single
    tau270 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau270(p) = tau270(p) + ( &
            v(p)**2 * t1(p)&
        )
    
    end do
    !$omp end do

    allocate(tau271(NAO))
    !$omp single
    tau271 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau271(p) = tau271(p) + ( &
                tau270(q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    allocate(tau272(NAO))
    !$omp single
    tau272 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau272(p) = tau272(p) + ( &
            t1(p) * tau271(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau273 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau273)
    
    do p=1, NAO
        tau273 = tau273 + ( &
            v(p)**2 * tau272(p)&
        )
    end do
    
    !$omp end do

    allocate(tau274(NAO))
    !$omp single
    tau274 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau274(p) = tau274(p) + ( &
            tau273 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau274(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau274)

    !$omp single
    tau317 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau317)
    
    do p=1, NAO
        tau317 = tau317 + ( &
            u(p)**2 * tau272(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau272)

    allocate(tau318(NAO))
    !$omp single
    tau318 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau318(p) = tau318(p) + ( &
            tau317 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau318(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau318)

    allocate(tau426(NAO))
    !$omp single
    tau426 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau426(p) = tau426(p) + ( &
            tau271(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau271)

    allocate(tau427(NAO))
    !$omp single
    tau427 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau427(p) = tau427(p) + ( &
            tau426(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau426)

    !$omp single
    tau428 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau428)
    
    do p=1, NAO
        tau428 = tau428 + ( &
            t1(p)**2 * tau427(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau427)

    allocate(tau429(NAO))
    !$omp single
    tau429 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau429(p) = tau429(p) + ( &
            tau428 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau429(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau429)

    allocate(tau356(NAO))
    !$omp single
    tau356 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau356(p) = tau356(p) + ( &
                tau270(q) * tau197(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau197)

    deallocate(tau270)

    allocate(tau357(NAO))
    !$omp single
    tau357 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau357(p) = tau357(p) + ( &
            t1(p) * tau356(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau356)

    !$omp single
    tau358 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau358)
    
    do p=1, NAO
        tau358 = tau358 + ( &
            u(p)**2 * tau357(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau357)

    allocate(tau359(NAO))
    !$omp single
    tau359 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau359(p) = tau359(p) + ( &
            tau358 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                4 * one(p) * tau359(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau359)

    allocate(tau284(NAO))
    !$omp single
    tau284 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau284(p) = tau284(p) + ( &
            t1(p)**2 * v(p)**2&
        )
    
    end do
    !$omp end do

    allocate(tau285(NAO))
    !$omp single
    tau285 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau285(p) = tau285(p) + ( &
                tau284(q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau284)

    allocate(tau286(NAO))
    !$omp single
    tau286 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau286(p) = tau286(p) + ( &
            u(p)**2 * tau285(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau285)

    !$omp single
    tau287 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau287)
    
    do p=1, NAO
        tau287 = tau287 + ( &
            t1(p)**2 * tau286(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau286)

    allocate(tau288(NAO))
    !$omp single
    tau288 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau288(p) = tau288(p) + ( &
            tau287 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau288(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau288)

    allocate(tau300(NAO))
    !$omp single
    tau300 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau300(p) = tau300(p) + ( &
            u(p)**2 * t2(p, p)&
        )
    
    end do
    !$omp end do

    allocate(tau301(NAO))
    !$omp single
    tau301 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau301(p) = tau301(p) + ( &
                tau300(q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau300)

    allocate(tau302(NAO))
    !$omp single
    tau302 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau302(p) = tau302(p) + ( &
            v(p)**2 * tau301(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau301)

    !$omp single
    tau303 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau303)
    
    do p=1, NAO
        tau303 = tau303 + ( &
            t1(p)**2 * tau302(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau302)

    allocate(tau304(NAO))
    !$omp single
    tau304 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau304(p) = tau304(p) + ( &
            tau303 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                one(p) * tau304(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau304)

    allocate(tau387(NAO))
    !$omp single
    tau387 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau387(p) = tau387(p) + ( &
            v(p) * t2(p, p)&
        )
    
    end do
    !$omp end do

    allocate(tau388(NAO))
    !$omp single
    tau388 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau388(p) = tau388(p) + ( &
            tau387(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau387)

    allocate(tau389(NAO))
    !$omp single
    tau389 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau389(p) = tau389(p) + ( &
                tau388(q) * z2(p, q)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau388)

    allocate(tau390(NAO))
    !$omp single
    tau390 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau390(p) = tau390(p) + ( &
            t1(p) * tau389(p)&
        )
    
    end do
    !$omp end do

    !$omp single
    tau391 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau391)
    
    do p=1, NAO
        tau391 = tau391 + ( &
            u(p)**2 * tau390(p)&
        )
    end do
    
    !$omp end do

    allocate(tau392(NAO))
    !$omp single
    tau392 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau392(p) = tau392(p) + ( &
            tau391 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau392(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau392)

    !$omp single
    tau442 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau442)
    
    do p=1, NAO
        tau442 = tau442 + ( &
            v(p)**2 * tau390(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau390)

    allocate(tau443(NAO))
    !$omp single
    tau443 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau443(p) = tau443(p) + ( &
            tau442 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau443(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau443)

    allocate(tau470(NAO))
    !$omp single
    tau470 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau470(p) = tau470(p) + ( &
            tau389(p) * t2(p, p)&
        )
    
    end do
    !$omp end do

    allocate(tau471(NAO))
    !$omp single
    tau471 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau471(p) = tau471(p) + ( &
            tau470(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau470)

    !$omp single
    tau472 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau472)
    
    do p=1, NAO
        tau472 = tau472 + ( &
            tau471(p) * u(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau471)

    allocate(tau473(NAO))
    !$omp single
    tau473 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau473(p) = tau473(p) + ( &
            tau472 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * tau473(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau473)

    allocate(tau485(NAO))
    !$omp single
    tau485 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau485(p) = tau485(p) + ( &
            tau389(p) * v(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau389)

    allocate(tau486(NAO))
    !$omp single
    tau486 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau486(p) = tau486(p) + ( &
            tau485(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau485)

    !$omp single
    tau487 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau487)
    
    do p=1, NAO
        tau487 = tau487 + ( &
            t1(p)**2 * tau486(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau486)

    allocate(tau488(NAO))
    !$omp single
    tau488 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau488(p) = tau488(p) + ( &
            tau487 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau488(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau488)

    allocate(tau407(NAO))
    !$omp single
    tau407 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau407(p) = tau407(p) + ( &
            v(p) * z2(p, p)&
        )
    
    end do
    !$omp end do

    allocate(tau408(NAO))
    !$omp single
    tau408 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau408(p) = tau408(p) + ( &
            tau407(p) * u(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau407)

    allocate(tau409(NAO))
    !$omp single
    tau409 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau409(p) = tau409(p) + ( &
            t1(p) * tau408(p)&
        )
    
    end do
    !$omp end do

    deallocate(tau408)

    allocate(tau410(NAO))
    !$omp single
    tau410 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
        do q=1, NAO
            tau410(p) = tau410(p) + ( &
                tau409(q) * t2(q, p)&
            )
        end do
    end do
    !$omp end do

    deallocate(tau409)

    !$omp single
    tau411 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau411)
    
    do p=1, NAO
        tau411 = tau411 + ( &
            v(p)**2 * tau410(p)&
        )
    end do
    
    !$omp end do

    allocate(tau412(NAO))
    !$omp single
    tau412 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau412(p) = tau412(p) + ( &
            tau411 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) - ( &
                2 * one(p) * tau412(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau412)

    !$omp single
    tau436 = 0.0
    !$omp end single

    !$omp do schedule(static) reduction(+:tau436)
    
    do p=1, NAO
        tau436 = tau436 + ( &
            u(p)**2 * tau410(p)&
        )
    end do
    
    !$omp end do

    deallocate(tau410)

    allocate(tau437(NAO))
    !$omp single
    tau437 = 0.0
    !$omp end single

    !$omp do schedule(static)
    do p=1, NAO
    
        tau437(p) = tau437(p) + ( &
            tau436 * one(p)&
        )
    
    end do
    !$omp end do

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                2 * one(p) * tau437(q)&
            )
    
        end do
    end do
    !$omp end do

    deallocate(tau437)

    !$omp do schedule(static)
    do q=1, NAO
        do p=1, NAO
    
            SpSq(p, q) = SpSq(p, q) + ( &
                one(p) * one(q) / 4&
            )
    
        end do
    end do
    !$omp end do

    !$omp end parallel
    end Subroutine CCSD_SpSq
    end Module 
