; ModuleID = 'program'
source_filename = "program"

define i32 @my_main(i32 %0) {
entry:
  %param0 = alloca i32, align 4
  store i32 %0, ptr %param0, align 4
  %load_param = load i32, ptr %param0, align 4
  %"%1" = alloca [2 x i32], align 4
  %gep = getelementptr [2 x i32], ptr %"%1", i32 0, i32 0
  store i32 %load_param, ptr %gep, align 4
  %gep1 = getelementptr [2 x i32], ptr %"%1", i32 0, i32 1
  store i32 42, ptr %gep1, align 4
  %arr = alloca ptr, align 8
  store ptr %"%1", ptr %arr, align 8
  %load_array_ptr = load ptr, ptr %arr, align 8
  %gep2 = getelementptr [2 x i32], ptr %load_array_ptr, i32 0, i32 0
  %"%3" = load i32, ptr %gep2, align 4
  %load_array_ptr3 = load ptr, ptr %arr, align 8
  %gep4 = getelementptr [2 x i32], ptr %load_array_ptr3, i32 0, i32 1
  %"%5" = load i32, ptr %gep4, align 4
  %"%6" = add i32 %"%3", %"%5"
  ret i32 %"%6"
}
