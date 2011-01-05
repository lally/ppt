; ModuleID = 'beta1.lli'

@_str1 = internal constant [17 x i8] c"Loading file %s\0A\00" ; <[17 x i8]*> [#uses=0]
@_str2 = internal constant [2 x i8] c"a\00"       ; <[2 x i8]*> [#uses=1]
@_str3 = internal constant [6 x i8] c"beta1\00"   ; <[6 x i8]*> [#uses=0]

declare i32 @printf(i8*, ...)

declare i32 @puts(i8*)

declare i32 @fprintf(i8*, i8*, ...)

declare i8* @fopen(i8*, i8*)

declare i32 @fclose(i8*)

declare i32 @fwrite(i8*, i32, i32, i8*)

declare i32 @fflush(i8*)

declare void @setbuf(i8*, i8*, i32)

declare i32 @atoi(i8*)

declare void @perror(i8*)

declare i8* @shmat(i32, i8*, i32)

declare void @usleep(i32)

define i32 @main(i32, i8**) {
_L1:
  %2 = alloca i32                                 ; <i32*> [#uses=5]
  %3 = alloca i32                                 ; <i32*> [#uses=5]
  %4 = alloca i32                                 ; <i32*> [#uses=5]
  %5 = alloca i32                                 ; <i32*> [#uses=1]
  %6 = alloca i32                                 ; <i32*> [#uses=4]
  %7 = alloca i32                                 ; <i32*> [#uses=3]
  %8 = alloca i32*                                ; <i32**> [#uses=7]
  %9 = alloca i32*                                ; <i32**> [#uses=3]
  %10 = alloca i8*                                ; <i8**> [#uses=3]
  %11 = icmp sge i32 %0, 2                        ; <i1> [#uses=1]
  br i1 %11, label %load_args, label %exit

exit:                                             ; preds = %load_args, %_L1
  ret i32 1

load_args:                                        ; preds = %_L1
  %12 = getelementptr i8** %1, i32 2              ; <i8**> [#uses=1]
  %13 = load i8** %12                             ; <i8*> [#uses=1]
  %14 = getelementptr i8* %13, i32 0              ; <i8*> [#uses=2]
  %15 = getelementptr i8** %1, i32 3              ; <i8**> [#uses=1]
  %16 = load i8** %15                             ; <i8*> [#uses=1]
  %17 = getelementptr i8* %16, i32 0              ; <i8*> [#uses=1]
  store i8* %14, i8** %10
  %18 = call i32 @atoi(i8* %14)                   ; <i32> [#uses=1]
  %19 = call i32 @atoi(i8* %17)                   ; <i32> [#uses=5]
  %20 = icmp sgt i32 %19, 0                       ; <i1> [#uses=1]
  br i1 %20, label %try_fopen, label %exit

try_fopen:                                        ; preds = %load_args
  %21 = getelementptr i8** %1, i32 1              ; <i8**> [#uses=1]
  %22 = load i8** %21                             ; <i8*> [#uses=1]
  %23 = getelementptr i8* %22, i32 0              ; <i8*> [#uses=2]
  store i8* %23, i8** %10
  %24 = call i8* @fopen(i8* %23, i8* getelementptr inbounds ([2 x i8]* @_str2, i32 0, i32 0)) ; <i8*> [#uses=2]
  %25 = icmp ne i8* %24, null                     ; <i1> [#uses=1]
  br i1 %25, label %try_shmat, label %p_exit

try_shmat:                                        ; preds = %try_fopen
  %26 = call i8* @shmat(i32 %18, i8* null, i32 256) ; <i8*> [#uses=2]
  %27 = bitcast i8* %26 to i32*                   ; <i32*> [#uses=3]
  %28 = icmp ne i8* %26, null                     ; <i1> [#uses=1]
  br i1 %28, label %init_vars, label %p_exit

init_vars:                                        ; preds = %try_shmat
  store i32* %27, i32** %8
  store i32* %27, i32** %9
  %29 = mul i32 %19, 2                            ; <i32> [#uses=2]
  %30 = getelementptr i32* %27, i32 %29           ; <i32*> [#uses=1]
  store i32 0, i32* %4
  store i32 0, i32* %5
  store i32 100000, i32* %6
  store i32 0, i32* %2
  br label %sleep_loop_head

p_exit:                                           ; preds = %try_shmat, %try_fopen
  %31 = load i8** %10                             ; <i8*> [#uses=1]
  call void @perror(i8* %31)
  ret i32 1

sleep_loop_head:                                  ; preds = %finish_read2, %init_vars
  store i32 0, i32* %3
  store i32 1, i32* %7
  %32 = load i32* %4                              ; <i32> [#uses=1]
  %33 = load i32** %8                             ; <i32*> [#uses=1]
  %34 = load i32* %33                             ; <i32> [#uses=1]
  %35 = icmp uge i32 %34, %32                     ; <i1> [#uses=1]
  br i1 %35, label %accept_frame, label %read_loop_head2

read_loop_head:                                   ; preds = %accept_frame, %failed_read
  %36 = load i32* %4                              ; <i32> [#uses=1]
  %37 = sub i32 %36, %29                          ; <i32> [#uses=2]
  %38 = icmp sle i32 %37, 0                       ; <i1> [#uses=1]
  %39 = select i1 %38, i32 %37, i32 0             ; <i32> [#uses=1]
  %40 = load i32** %8                             ; <i32*> [#uses=1]
  %41 = load i32* %40                             ; <i32> [#uses=1]
  %42 = icmp ule i32 %41, %39                     ; <i1> [#uses=1]
  br i1 %42, label %accept_frame, label %read_loop_head3

read_loop_head2:                                  ; preds = %sleep_loop_head
  %43 = load i32* %3                              ; <i32> [#uses=1]
  %44 = icmp eq i32 %43, 0                        ; <i1> [#uses=1]
  %45 = load i32** %8                             ; <i32*> [#uses=1]
  %46 = load i32* %45                             ; <i32> [#uses=1]
  %47 = load i32* %4                              ; <i32> [#uses=1]
  %48 = icmp ne i32 %46, %47                      ; <i1> [#uses=1]
  %49 = and i1 %44, %48                           ; <i1> [#uses=1]
  br i1 %49, label %accept_frame, label %failed_read

read_loop_head3:                                  ; preds = %read_loop_head
  store i32 0, i32* %7
  br label %write_buffer

failed_read:                                      ; preds = %read_loop_head2
  %50 = load i32* %2                              ; <i32> [#uses=1]
  %51 = add i32 %50, 1                            ; <i32> [#uses=1]
  store i32 %51, i32* %2
  %52 = load i32* %3                              ; <i32> [#uses=1]
  %53 = add i32 %52, 1                            ; <i32> [#uses=1]
  store i32 %53, i32* %3
  %54 = load i32** %8                             ; <i32*> [#uses=2]
  %55 = load i32* %54                             ; <i32> [#uses=1]
  store i32 %55, i32* %4
  %56 = getelementptr i32* %54, i32 2             ; <i32*> [#uses=2]
  store i32* %56, i32** %8
  %57 = ptrtoint i32* %56 to i32                  ; <i32> [#uses=1]
  %58 = ptrtoint i32* %30 to i32                  ; <i32> [#uses=1]
  %59 = icmp eq i32 %57, %58                      ; <i1> [#uses=1]
  br i1 %59, label %write_buffer, label %read_loop_head

accept_frame:                                     ; preds = %read_loop_head2, %read_loop_head, %sleep_loop_head
  %60 = load i32** %9                             ; <i32*> [#uses=1]
  %61 = bitcast i32* %60 to i8*                   ; <i8*> [#uses=1]
  %62 = load i32* %2                              ; <i32> [#uses=1]
  %63 = call i32 @fwrite(i8* %61, i32 %62, i32 8, i8* %24) ; <i32> [#uses=0]
  %64 = load i32** %8                             ; <i32*> [#uses=1]
  store i32* %64, i32** %9
  store i32 0, i32* %2
  %65 = load i32* %7                              ; <i32> [#uses=1]
  %66 = icmp eq i32 %65, 0                        ; <i1> [#uses=1]
  br i1 %66, label %finish_read, label %read_loop_head

finish_frames:                                    ; No predecessors!
  %67 = load i32* %3                              ; <i32> [#uses=2]
  %68 = sub i32 %67, %19                          ; <i32> [#uses=2]
  %69 = ashr i32 %19, 3                           ; <i32> [#uses=2]
  %70 = icmp sle i32 %68, %69                     ; <i1> [#uses=1]
  br i1 %70, label %recalc, label %finish_read2

write_buffer:                                     ; preds = %failed_read, %read_loop_head3
  %71 = mul i32 %69, 7                            ; <i32> [#uses=1]
  %72 = icmp sge i32 %68, %71                     ; <i1> [#uses=1]
  br i1 %72, label %recalc, label %do_sleep

finish_read:                                      ; preds = %accept_frame
  %73 = load i32* %6                              ; <i32> [#uses=2]
  %74 = mul i32 %19, %73                          ; <i32> [#uses=1]
  %75 = mul i32 %67, 2                            ; <i32> [#uses=1]
  %76 = udiv i32 %74, %75                         ; <i32> [#uses=2]
  %77 = icmp sle i32 %76, 2000000                 ; <i1> [#uses=1]
  %78 = select i1 %77, i32 %76, i32 2000000       ; <i32> [#uses=2]
  %79 = icmp sge i32 %78, 100000                  ; <i1> [#uses=1]
  %80 = select i1 %79, i32 %78, i32 100000        ; <i32> [#uses=1]
  store i32 %80, i32* %6
  br label %do_sleep

finish_read2:                                     ; preds = %finish_frames
  %81 = load i32* %6                              ; <i32> [#uses=0]
  call void @usleep(i32 %73)
  br label %sleep_loop_head

recalc:                                           ; preds = %write_buffer, %finish_frames

do_sleep:                                         ; preds = %finish_read, %write_buffer
}
