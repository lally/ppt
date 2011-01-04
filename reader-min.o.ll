; ModuleID = 'reader-min.o'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32"
target triple = "i386-pc-solaris2.11"

%struct.__FILE = type { i32, i8*, i8*, i8, i8, i8, i8 }
%struct.anon_map = type opaque
%struct.ipc_perm = type { i32, i32, i32, i32, i32, i32, i32, [4 x i32] }
%struct.metrics_beta1_frame = type { i32, double }
%struct.shmid_ds = type { %struct.ipc_perm, i32, %struct.anon_map*, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, [4 x i32] }

@dest = global %struct.__FILE* null, align 4      ; <%struct.__FILE**> [#uses=7]
@.str = private constant [10 x i8] c"MAX_SEQNO\00" ; <[10 x i8]*> [#uses=1]
@.str1 = private constant [14 x i8] c"max_seqno=%f\0A\00" ; <[14 x i8]*> [#uses=1]
@.str2 = private constant [26 x i8] c"usage: %s <shmid> output\0A\00" ; <[26 x i8]*> [#uses=1]
@.str3 = private constant [8 x i8] c"where:\0A\00" ; <[8 x i8]*> [#uses=1]
@.str4 = private constant [36 x i8] c"   shmid is a shared memory handle\0A\00" ; <[36 x i8]*> [#uses=1]
@.str5 = private constant [39 x i8] c"   output is the destination filename\0A\00" ; <[39 x i8]*> [#uses=1]
@.str6 = private constant [14 x i8] c"invalid shmid\00" ; <[14 x i8]*> [#uses=1]
@.str7 = private constant [3 x i8] c"w+\00"       ; <[3 x i8]*> [#uses=1]
@.str8 = private constant [6 x i8] c"shmat\00"    ; <[6 x i8]*> [#uses=1]
@.str9 = private constant [7 x i8] c"beta1\0A\00" ; <[7 x i8]*> [#uses=1]
@.str10 = private constant [16 x i8] c"last_seqno >= 0\00" ; <[16 x i8]*> [#uses=1]
@.str11 = private constant [15 x i8] c"reader-min.cpp\00" ; <[15 x i8]*> [#uses=1]
@.str12 = private constant [2 x i8] c".\00"       ; <[2 x i8]*> [#uses=1]
@.str13 = private constant [7 x i8] c"%10.5f\00"  ; <[7 x i8]*> [#uses=1]

define i32 @_Z3minmm(i32 %a, i32 %b) {
entry:
  %retval = alloca i32, align 4                   ; <i32*> [#uses=2]
  %a.addr = alloca i32, align 4                   ; <i32*> [#uses=3]
  %b.addr = alloca i32, align 4                   ; <i32*> [#uses=3]
  store i32 %a, i32* %a.addr
  store i32 %b, i32* %b.addr
  %tmp = load i32* %a.addr                        ; <i32> [#uses=1]
  %tmp1 = load i32* %b.addr                       ; <i32> [#uses=1]
  %cmp = icmp ult i32 %tmp, %tmp1                 ; <i1> [#uses=1]
  %tmp2 = load i32* %a.addr                       ; <i32> [#uses=1]
  %tmp3 = load i32* %b.addr                       ; <i32> [#uses=1]
  %cond = select i1 %cmp, i32 %tmp2, i32 %tmp3    ; <i32> [#uses=1]
  store i32 %cond, i32* %retval
  %0 = load i32* %retval                          ; <i32> [#uses=1]
  ret i32 %0
}

define i32 @main(i32 %args, i8** %argv) {
entry:
  %retval = alloca i32, align 4                   ; <i32*> [#uses=2]
  %args.addr = alloca i32, align 4                ; <i32*> [#uses=2]
  %argv.addr = alloca i8**, align 4               ; <i8***> [#uses=6]
  %shmid = alloca i32, align 4                    ; <i32*> [#uses=4]
  %size = alloca i32, align 4                     ; <i32*> [#uses=7]
  %buf = alloca i8*, align 4                      ; <i8**> [#uses=2]
  %delay = alloca i32, align 4                    ; <i32*> [#uses=9]
  %start = alloca %struct.metrics_beta1_frame*, align 4 ; <%struct.metrics_beta1_frame**> [#uses=5]
  %cur = alloca %struct.metrics_beta1_frame*, align 4 ; <%struct.metrics_beta1_frame**> [#uses=9]
  %end = alloca %struct.metrics_beta1_frame*, align 4 ; <%struct.metrics_beta1_frame**> [#uses=2]
  %last_seqno = alloca i32, align 4               ; <i32*> [#uses=6]
  %max_seqno = alloca i32, align 4                ; <i32*> [#uses=3]
  %e_max = alloca i8*, align 4                    ; <i8**> [#uses=3]
  %shm_inf = alloca %struct.shmid_ds, align 4     ; <%struct.shmid_ds*> [#uses=2]
  %seqno = alloca i32, align 4                    ; <i32*> [#uses=4]
  %count = alloca i32, align 4                    ; <i32*> [#uses=8]
  %recalc_delay = alloca i8, align 1              ; <i8*> [#uses=3]
  %rate = alloca double, align 8                  ; <double*> [#uses=2]
  %new_delay = alloca double, align 8             ; <double*> [#uses=2]
  store i32 0, i32* %retval
  store i32 %args, i32* %args.addr
  store i8** %argv, i8*** %argv.addr
  store i32 -1, i32* %max_seqno
  %call = call i8* @getenv(i8* getelementptr inbounds ([10 x i8]* @.str, i32 0, i32 0)) ; <i8*> [#uses=1]
  store i8* %call, i8** %e_max
  %tmp = load i8** %e_max                         ; <i8*> [#uses=1]
  %tobool = icmp ne i8* %tmp, null                ; <i1> [#uses=1]
  br i1 %tobool, label %if.then, label %if.end

if.then:                                          ; preds = %entry
  %tmp1 = load i8** %e_max                        ; <i8*> [#uses=1]
  %call2 = call double @atof(i8* %tmp1)           ; <double> [#uses=1]
  %conv = fptoui double %call2 to i32             ; <i32> [#uses=1]
  store i32 %conv, i32* %max_seqno
  br label %if.end

if.end:                                           ; preds = %if.then, %entry
  %tmp3 = load i32* %max_seqno                    ; <i32> [#uses=1]
  %conv4 = uitofp i32 %tmp3 to double             ; <double> [#uses=1]
  %call5 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([14 x i8]* @.str1, i32 0, i32 0), double %conv4) ; <i32> [#uses=0]
  store i32 0, i32* %last_seqno
  store i32 1000, i32* %delay
  %tmp6 = load i32* %args.addr                    ; <i32> [#uses=1]
  %cmp = icmp slt i32 %tmp6, 3                    ; <i1> [#uses=1]
  br i1 %cmp, label %if.then7, label %if.end14

if.then7:                                         ; preds = %if.end
  %tmp8 = load i8*** %argv.addr                   ; <i8**> [#uses=1]
  %arrayidx = getelementptr inbounds i8** %tmp8, i32 0 ; <i8**> [#uses=1]
  %tmp9 = load i8** %arrayidx                     ; <i8*> [#uses=1]
  %call10 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([26 x i8]* @.str2, i32 0, i32 0), i8* %tmp9) ; <i32> [#uses=0]
  %call11 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([8 x i8]* @.str3, i32 0, i32 0)) ; <i32> [#uses=0]
  %call12 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([36 x i8]* @.str4, i32 0, i32 0)) ; <i32> [#uses=0]
  %call13 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([39 x i8]* @.str5, i32 0, i32 0)) ; <i32> [#uses=0]
  call void @exit(i32 1) noreturn
  unreachable

if.end14:                                         ; preds = %if.end
  %tmp15 = load i8*** %argv.addr                  ; <i8**> [#uses=1]
  %arrayidx16 = getelementptr inbounds i8** %tmp15, i32 1 ; <i8**> [#uses=1]
  %tmp17 = load i8** %arrayidx16                  ; <i8*> [#uses=1]
  %call18 = call i32 @atoi(i8* %tmp17)            ; <i32> [#uses=1]
  store i32 %call18, i32* %shmid
  %tmp19 = load i32* %shmid                       ; <i32> [#uses=1]
  %cmp20 = icmp sle i32 %tmp19, 0                 ; <i1> [#uses=1]
  br i1 %cmp20, label %if.then21, label %if.end23

if.then21:                                        ; preds = %if.end14
  %call22 = call i32 @puts(i8* getelementptr inbounds ([14 x i8]* @.str6, i32 0, i32 0)) ; <i32> [#uses=0]
  call void @exit(i32 1) noreturn
  unreachable

if.end23:                                         ; preds = %if.end14
  %tmp25 = load i32* %shmid                       ; <i32> [#uses=1]
  %call26 = call i32 @shmctl(i32 %tmp25, i32 12, %struct.shmid_ds* %shm_inf) ; <i32> [#uses=1]
  %cmp27 = icmp ne i32 0, %call26                 ; <i1> [#uses=1]
  br i1 %cmp27, label %if.then28, label %if.end32

if.then28:                                        ; preds = %if.end23
  %tmp29 = load i8*** %argv.addr                  ; <i8**> [#uses=1]
  %arrayidx30 = getelementptr inbounds i8** %tmp29, i32 1 ; <i8**> [#uses=1]
  %tmp31 = load i8** %arrayidx30                  ; <i8*> [#uses=1]
  call void @perror(i8* %tmp31)
  call void @exit(i32 1) noreturn
  unreachable

if.end32:                                         ; preds = %if.end23
  %tmp33 = getelementptr inbounds %struct.shmid_ds* %shm_inf, i32 0, i32 1 ; <i32*> [#uses=1]
  %tmp34 = load i32* %tmp33                       ; <i32> [#uses=1]
  %div = udiv i32 %tmp34, 12                      ; <i32> [#uses=1]
  store i32 %div, i32* %size
  %tmp35 = load i8*** %argv.addr                  ; <i8**> [#uses=1]
  %arrayidx36 = getelementptr inbounds i8** %tmp35, i32 2 ; <i8**> [#uses=1]
  %tmp37 = load i8** %arrayidx36                  ; <i8*> [#uses=1]
  %call38 = call %struct.__FILE* @fopen(i8* %tmp37, i8* getelementptr inbounds ([3 x i8]* @.str7, i32 0, i32 0)) ; <%struct.__FILE*> [#uses=1]
  store %struct.__FILE* %call38, %struct.__FILE** @dest
  %tmp39 = load %struct.__FILE** @dest            ; <%struct.__FILE*> [#uses=1]
  %cmp40 = icmp eq %struct.__FILE* null, %tmp39   ; <i1> [#uses=1]
  br i1 %cmp40, label %if.then41, label %if.end45

if.then41:                                        ; preds = %if.end32
  %tmp42 = load i8*** %argv.addr                  ; <i8**> [#uses=1]
  %arrayidx43 = getelementptr inbounds i8** %tmp42, i32 2 ; <i8**> [#uses=1]
  %tmp44 = load i8** %arrayidx43                  ; <i8*> [#uses=1]
  call void @perror(i8* %tmp44)
  call void @exit(i32 1) noreturn
  unreachable

if.end45:                                         ; preds = %if.end32
  %tmp46 = load i32* %shmid                       ; <i32> [#uses=1]
  %call47 = call i8* @shmat(i32 %tmp46, i8* null, i32 0) ; <i8*> [#uses=1]
  %0 = bitcast i8* %call47 to %struct.metrics_beta1_frame* ; <%struct.metrics_beta1_frame*> [#uses=1]
  store %struct.metrics_beta1_frame* %0, %struct.metrics_beta1_frame** %start
  %tmp48 = load %struct.metrics_beta1_frame** %start ; <%struct.metrics_beta1_frame*> [#uses=1]
  %cmp49 = icmp eq %struct.metrics_beta1_frame* %tmp48, inttoptr (i32 -1 to %struct.metrics_beta1_frame*) ; <i1> [#uses=1]
  br i1 %cmp49, label %if.then50, label %if.end51

if.then50:                                        ; preds = %if.end45
  call void @perror(i8* getelementptr inbounds ([6 x i8]* @.str8, i32 0, i32 0))
  call void @exit(i32 1) noreturn
  unreachable

if.end51:                                         ; preds = %if.end45
  %tmp52 = load %struct.metrics_beta1_frame** %start ; <%struct.metrics_beta1_frame*> [#uses=1]
  %tmp53 = load i32* %size                        ; <i32> [#uses=1]
  %add.ptr = getelementptr inbounds %struct.metrics_beta1_frame* %tmp52, i32 %tmp53 ; <%struct.metrics_beta1_frame*> [#uses=1]
  store %struct.metrics_beta1_frame* %add.ptr, %struct.metrics_beta1_frame** %end
  %tmp54 = load %struct.metrics_beta1_frame** %start ; <%struct.metrics_beta1_frame*> [#uses=1]
  store %struct.metrics_beta1_frame* %tmp54, %struct.metrics_beta1_frame** %cur
  %call55 = call i8* @malloc(i32 65536)           ; <i8*> [#uses=1]
  store i8* %call55, i8** %buf
  %tmp56 = load %struct.__FILE** @dest            ; <%struct.__FILE*> [#uses=1]
  %tmp57 = load i8** %buf                         ; <i8*> [#uses=1]
  %call58 = call i32 @setvbuf(%struct.__FILE* %tmp56, i8* %tmp57, i32 0, i32 65536) ; <i32> [#uses=0]
  %tmp59 = load %struct.__FILE** @dest            ; <%struct.__FILE*> [#uses=1]
  %call60 = call i32 (%struct.__FILE*, i8*, ...)* @fprintf(%struct.__FILE* %tmp59, i8* getelementptr inbounds ([7 x i8]* @.str9, i32 0, i32 0)) ; <i32> [#uses=0]
  br label %while.cond

while.cond:                                       ; preds = %if.end155, %if.end51
  %tmp61 = load %struct.__FILE** @dest            ; <%struct.__FILE*> [#uses=1]
  %tobool62 = icmp ne %struct.__FILE* %tmp61, null ; <i1> [#uses=1]
  br i1 %tobool62, label %while.body, label %while.end159

while.body:                                       ; preds = %while.cond
  store i32 0, i32* %count
  %tmp65 = load i32* %last_seqno                  ; <i32> [#uses=1]
  store i32 %tmp65, i32* %seqno
  %tmp66 = load i32* %last_seqno                  ; <i32> [#uses=1]
  %cmp67 = icmp uge i32 %tmp66, 0                 ; <i1> [#uses=1]
  br i1 %cmp67, label %lor.end, label %lor.rhs

lor.rhs:                                          ; preds = %while.body
  call void @__assert(i8* getelementptr inbounds ([16 x i8]* @.str10, i32 0, i32 0), i8* getelementptr inbounds ([15 x i8]* @.str11, i32 0, i32 0), i32 85)
  br label %lor.end

lor.end:                                          ; preds = %lor.rhs, %while.body
  %1 = phi i1 [ true, %while.body ], [ false, %lor.rhs ] ; <i1> [#uses=0]
  br label %while.cond68

while.cond68:                                     ; preds = %if.end110, %lor.end
  %tmp69 = load %struct.metrics_beta1_frame** %cur ; <%struct.metrics_beta1_frame*> [#uses=1]
  %tmp70 = getelementptr inbounds %struct.metrics_beta1_frame* %tmp69, i32 0, i32 0 ; <i32*> [#uses=1]
  %tmp71 = load i32* %tmp70                       ; <i32> [#uses=1]
  %tmp72 = load i32* %seqno                       ; <i32> [#uses=1]
  %cmp73 = icmp ugt i32 %tmp71, %tmp72            ; <i1> [#uses=1]
  br i1 %cmp73, label %lor.end89, label %lor.lhs.false

lor.lhs.false:                                    ; preds = %while.cond68
  %tmp74 = load %struct.metrics_beta1_frame** %cur ; <%struct.metrics_beta1_frame*> [#uses=1]
  %tmp75 = getelementptr inbounds %struct.metrics_beta1_frame* %tmp74, i32 0, i32 0 ; <i32*> [#uses=1]
  %tmp76 = load i32* %tmp75                       ; <i32> [#uses=1]
  %tmp77 = load i32* %last_seqno                  ; <i32> [#uses=1]
  %tmp78 = load i32* %size                        ; <i32> [#uses=1]
  %sub = sub i32 %tmp77, %tmp78                   ; <i32> [#uses=1]
  %call79 = call i32 @_Z3minmm(i32 %sub, i32 0)   ; <i32> [#uses=1]
  %cmp80 = icmp ule i32 %tmp76, %call79           ; <i1> [#uses=1]
  br i1 %cmp80, label %lor.end89, label %lor.rhs81

lor.rhs81:                                        ; preds = %lor.lhs.false
  %tmp82 = load i32* %count                       ; <i32> [#uses=1]
  %cmp83 = icmp eq i32 %tmp82, 0                  ; <i1> [#uses=1]
  br i1 %cmp83, label %land.rhs, label %land.end

land.rhs:                                         ; preds = %lor.rhs81
  %tmp84 = load %struct.metrics_beta1_frame** %cur ; <%struct.metrics_beta1_frame*> [#uses=1]
  %tmp85 = getelementptr inbounds %struct.metrics_beta1_frame* %tmp84, i32 0, i32 0 ; <i32*> [#uses=1]
  %tmp86 = load i32* %tmp85                       ; <i32> [#uses=1]
  %tmp87 = load i32* %last_seqno                  ; <i32> [#uses=1]
  %cmp88 = icmp ne i32 %tmp86, %tmp87             ; <i1> [#uses=1]
  br label %land.end

land.end:                                         ; preds = %land.rhs, %lor.rhs81
  %2 = phi i1 [ false, %lor.rhs81 ], [ %cmp88, %land.rhs ] ; <i1> [#uses=1]
  br label %lor.end89

lor.end89:                                        ; preds = %land.end, %lor.lhs.false, %while.cond68
  %3 = phi i1 [ true, %lor.lhs.false ], [ true, %while.cond68 ], [ %2, %land.end ] ; <i1> [#uses=1]
  br i1 %3, label %while.body90, label %while.end

while.body90:                                     ; preds = %lor.end89
  %call91 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([2 x i8]* @.str12, i32 0, i32 0)) ; <i32> [#uses=0]
  %tmp92 = load %struct.__FILE** @dest            ; <%struct.__FILE*> [#uses=1]
  %tobool93 = icmp ne %struct.__FILE* %tmp92, null ; <i1> [#uses=1]
  br i1 %tobool93, label %if.then94, label %if.end100

if.then94:                                        ; preds = %while.body90
  %tmp95 = load %struct.__FILE** @dest            ; <%struct.__FILE*> [#uses=1]
  %tmp96 = load %struct.metrics_beta1_frame** %cur ; <%struct.metrics_beta1_frame*> [#uses=1]
  %tmp97 = getelementptr inbounds %struct.metrics_beta1_frame* %tmp96, i32 0, i32 1 ; <double*> [#uses=1]
  %tmp98 = load double* %tmp97                    ; <double> [#uses=1]
  %call99 = call i32 (%struct.__FILE*, i8*, ...)* @fprintf(%struct.__FILE* %tmp95, i8* getelementptr inbounds ([7 x i8]* @.str13, i32 0, i32 0), double %tmp98) ; <i32> [#uses=0]
  br label %if.end100

if.end100:                                        ; preds = %if.then94, %while.body90
  %tmp101 = load %struct.metrics_beta1_frame** %cur ; <%struct.metrics_beta1_frame*> [#uses=1]
  %tmp102 = getelementptr inbounds %struct.metrics_beta1_frame* %tmp101, i32 0, i32 0 ; <i32*> [#uses=1]
  %tmp103 = load i32* %tmp102                     ; <i32> [#uses=1]
  store i32 %tmp103, i32* %seqno
  %tmp104 = load i32* %count                      ; <i32> [#uses=1]
  %inc = add i32 %tmp104, 1                       ; <i32> [#uses=1]
  store i32 %inc, i32* %count
  %tmp105 = load %struct.metrics_beta1_frame** %cur ; <%struct.metrics_beta1_frame*> [#uses=1]
  %ptrincdec = getelementptr inbounds %struct.metrics_beta1_frame* %tmp105, i32 1 ; <%struct.metrics_beta1_frame*> [#uses=2]
  store %struct.metrics_beta1_frame* %ptrincdec, %struct.metrics_beta1_frame** %cur
  %tmp106 = load %struct.metrics_beta1_frame** %end ; <%struct.metrics_beta1_frame*> [#uses=1]
  %cmp107 = icmp uge %struct.metrics_beta1_frame* %ptrincdec, %tmp106 ; <i1> [#uses=1]
  br i1 %cmp107, label %if.then108, label %if.end110

if.then108:                                       ; preds = %if.end100
  %tmp109 = load %struct.metrics_beta1_frame** %start ; <%struct.metrics_beta1_frame*> [#uses=1]
  store %struct.metrics_beta1_frame* %tmp109, %struct.metrics_beta1_frame** %cur
  br label %if.end110

if.end110:                                        ; preds = %if.then108, %if.end100
  br label %while.cond68

while.end:                                        ; preds = %lor.end89
  %tmp111 = load i32* %seqno                      ; <i32> [#uses=1]
  store i32 %tmp111, i32* %last_seqno
  store i8 0, i8* %recalc_delay
  %tmp113 = load i32* %count                      ; <i32> [#uses=1]
  %tmp114 = load i32* %size                       ; <i32> [#uses=1]
  %tmp115 = load i32* %size                       ; <i32> [#uses=1]
  %div116 = sdiv i32 %tmp115, 8                   ; <i32> [#uses=1]
  %sub117 = sub i32 %tmp114, %div116              ; <i32> [#uses=1]
  %cmp118 = icmp ugt i32 %tmp113, %sub117         ; <i1> [#uses=1]
  br i1 %cmp118, label %if.then124, label %lor.lhs.false119

lor.lhs.false119:                                 ; preds = %while.end
  %tmp120 = load i32* %count                      ; <i32> [#uses=1]
  %tmp121 = load i32* %size                       ; <i32> [#uses=1]
  %div122 = sdiv i32 %tmp121, 8                   ; <i32> [#uses=1]
  %cmp123 = icmp ult i32 %tmp120, %div122         ; <i1> [#uses=1]
  br i1 %cmp123, label %if.then124, label %if.end125

if.then124:                                       ; preds = %lor.lhs.false119, %while.end
  store i8 1, i8* %recalc_delay
  br label %if.end125

if.end125:                                        ; preds = %if.then124, %lor.lhs.false119
  %tmp126 = load i32* %count                      ; <i32> [#uses=1]
  %tobool127 = icmp ne i32 %tmp126, 0             ; <i1> [#uses=1]
  br i1 %tobool127, label %if.else, label %if.then128

if.then128:                                       ; preds = %if.end125
  store i32 2000, i32* %delay
  br label %if.end146

if.else:                                          ; preds = %if.end125
  %tmp129 = load i8* %recalc_delay                ; <i8> [#uses=1]
  %tobool130 = trunc i8 %tmp129 to i1             ; <i1> [#uses=1]
  br i1 %tobool130, label %if.then131, label %if.end145

if.then131:                                       ; preds = %if.else
  %tmp133 = load i32* %count                      ; <i32> [#uses=1]
  %conv134 = uitofp i32 %tmp133 to double         ; <double> [#uses=1]
  %add = fadd double 0.000000e+00, %conv134       ; <double> [#uses=1]
  %tmp135 = load i32* %delay                      ; <i32> [#uses=1]
  %conv136 = sitofp i32 %tmp135 to double         ; <double> [#uses=1]
  %div137 = fdiv double %add, %conv136            ; <double> [#uses=1]
  store double %div137, double* %rate
  %tmp139 = load i32* %size                       ; <i32> [#uses=1]
  %conv140 = sitofp i32 %tmp139 to double         ; <double> [#uses=1]
  %mul = fmul double %conv140, 5.000000e-01       ; <double> [#uses=1]
  %tmp141 = load double* %rate                    ; <double> [#uses=1]
  %div142 = fdiv double %mul, %tmp141             ; <double> [#uses=1]
  store double %div142, double* %new_delay
  %tmp143 = load double* %new_delay               ; <double> [#uses=1]
  %conv144 = fptosi double %tmp143 to i32         ; <i32> [#uses=1]
  store i32 %conv144, i32* %delay
  br label %if.end145

if.end145:                                        ; preds = %if.then131, %if.else
  br label %if.end146

if.end146:                                        ; preds = %if.end145, %if.then128
  %tmp147 = load i32* %delay                      ; <i32> [#uses=1]
  %cmp148 = icmp slt i32 %tmp147, 100             ; <i1> [#uses=1]
  br i1 %cmp148, label %if.then149, label %if.else150

if.then149:                                       ; preds = %if.end146
  store i32 100, i32* %delay
  br label %if.end155

if.else150:                                       ; preds = %if.end146
  %tmp151 = load i32* %delay                      ; <i32> [#uses=1]
  %cmp152 = icmp sgt i32 %tmp151, 2000            ; <i1> [#uses=1]
  br i1 %cmp152, label %if.then153, label %if.end154

if.then153:                                       ; preds = %if.else150
  store i32 2000, i32* %delay
  br label %if.end154

if.end154:                                        ; preds = %if.then153, %if.else150
  br label %if.end155

if.end155:                                        ; preds = %if.end154, %if.then149
  %tmp156 = load i32* %delay                      ; <i32> [#uses=1]
  %mul157 = mul i32 %tmp156, 1000                 ; <i32> [#uses=1]
  %call158 = call i32 @usleep(i32 %mul157)        ; <i32> [#uses=0]
  br label %while.cond

while.end159:                                     ; preds = %while.cond
  %4 = load i32* %retval                          ; <i32> [#uses=1]
  ret i32 %4
}

declare i8* @getenv(i8*)

declare double @atof(i8*)

declare i32 @printf(i8*, ...)

declare void @exit(i32) noreturn

declare i32 @atoi(i8*)

declare i32 @puts(i8*)

declare i32 @shmctl(i32, i32, %struct.shmid_ds*)

declare void @perror(i8*)

declare %struct.__FILE* @fopen(i8*, i8*)

declare i8* @shmat(i32, i8*, i32)

declare i8* @malloc(i32)

declare i32 @setvbuf(%struct.__FILE*, i8*, i32, i32)

declare i32 @fprintf(%struct.__FILE*, i8*, ...)

declare void @__assert(i8*, i8*, i32)

declare i32 @usleep(i32)
