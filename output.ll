; ModuleID = 'program'
source_filename = "program"

define i32 @my_main(i32 %0) {
entry:
  %x = alloca i32, align 4
  store i32 %0, ptr %x, align 4
  %load_left = load i32, ptr %x, align 4
  %"%1" = icmp sgt i32 %load_left, 5
  br i1 %"%1", label %L0, label %L1

L0:                                               ; preds = %entry
  %load_left1 = load i32, ptr %x, align 4
  %"%3" = add i32 %load_left1, 2
  store i32 %"%3", ptr %x, align 4
  %load_left2 = load i32, ptr %x, align 4
  %"%5" = icmp slt i32 %load_left2, 10
  br i1 %"%5", label %L3, label %L4

L3:                                               ; preds = %L0
  store i32 8, ptr %x, align 4
  br label %L5

L4:                                               ; preds = %L0
  store i32 12, ptr %x, align 4
  br label %L5

L5:                                               ; preds = %L4, %L3
  br label %L2

L1:                                               ; preds = %entry
  %load_left3 = load i32, ptr %x, align 4
  %"%9" = mul i32 %load_left3, 3
  store i32 %"%9", ptr %x, align 4
  br label %L2

L2:                                               ; preds = %L1, %L5
  %load_ret = load i32, ptr %x, align 4
  ret i32 %load_ret
}
