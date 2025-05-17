; ModuleID = 'main'
source_filename = "main"

@0 = private unnamed_addr constant [17 x i8] c"Hello from Gran!\00", align 1
@1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@2 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@3 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@4 = private unnamed_addr constant [12 x i8] c"The sum is:\00", align 1
@5 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@6 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

define i32 @main() {
entry:
  %0 = call i32 (ptr, ...) @screenit(ptr @1, ptr @0)
  %x = alloca i32, align 4
  store i32 42, ptr %x, align 4
  %x1 = load i32, ptr %x, align 4
  %1 = call i32 (ptr, ...) @screenit(ptr @2, i32 %x1)
  %y = alloca i32, align 4
  store i32 15, ptr %y, align 4
  %y2 = load i32, ptr %y, align 4
  %2 = call i32 (ptr, ...) @screenit(ptr @3, i32 %y2)
  %3 = call i32 (ptr, ...) @screenit(ptr @5, ptr @4)
  %x3 = load i32, ptr %x, align 4
  %y4 = load i32, ptr %y, align 4
  %addtmp = add i32 %x3, %y4
  %4 = call i32 (ptr, ...) @screenit(ptr @6, i32 %addtmp)
  ret i32 0
}

declare i32 @screenit(ptr, ...)
