; ModuleID = 'examples/echo.ff'


 


declare external ccc  i32 @printf(i8*, ...)    


declare external ccc  i32 @scanf(i8*, ...)    


declare external ccc  i32 @puts(i8*)    


declare external ccc  i32 @putchar(i8)    


declare external ccc  void @perror(i8*)    


declare external ccc  i32 @socket(i32, i32, i32)    


declare external ccc  i32 @connect(i32, i8*, i32)    


declare external ccc  i32 @bind(i32, i8*, i32)    


declare external ccc  i32 @listen(i32, i32)    


declare external ccc  i32 @accept(i32, i8*, i32*)    


declare external ccc  i32 @send(i32, i8*, i32, i32)    


declare external ccc  i32 @recv(i32, i8*, i32, i32)    


declare external ccc  i32 @sendto(i32, i8*, i32, i32, i8*, i32)    


declare external ccc  i32 @recvfrom(i32, i8*, i32, i32, i8*, i32*)    


declare external ccc  i32 @shutdown(i32, i32)    


declare external ccc  i32 @setsockopt(i32, i32, i32, i8*, i32)    


declare external ccc  i32 @getsockopt(i32, i32, i32, i8*, i32*)    


declare external ccc  i32 @atoi(i8*)    


declare external ccc  i32 @atol(i8*)    


declare external ccc  double @atof(i8*)    


declare external ccc  i32 @abs(i32)    


declare external ccc  i32 @labs(i32)    


declare external ccc  i32 @strtol(i8*, i8*, i32)    


declare external ccc  i32 @strtoll(i8*, i8*, i32)    


declare external ccc  double @strtod(i8*, i8*)    


declare external ccc  void @srand(i32)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i8* @malloc(i32)    


declare external ccc  i8* @realloc(i8*, i32)    


declare external ccc  void @free(i8*)    


declare external ccc  i64 @htonll(i64)    


declare external ccc  i32 @htonl(i32)    


declare external ccc  i16 @htons(i16)    


declare external ccc  i64 @ntohll(i64)    


declare external ccc  i32 @ntohl(i32)    


declare external ccc  i16 @ntohs(i16)    


declare external ccc  i32 @inet_addr(i8*)    


declare external ccc  i32 @inet_aton(i8*, i8*)    


declare external ccc  i8* @inet_ntoa(i32)    


declare external ccc  i8* @inet_ntop(i32, i8*, i8*, i32)    


declare external ccc  i32 @inet_pton(i32, i8*, i8*)    


declare external ccc  i32 @write(i32, i8*, i32)    


declare external ccc  i32 @read(i32, i8*, i32)    


declare external ccc  i32 @close(i32)    


declare external ccc  i32 @unlink(i8*)    


declare external ccc  i32 @access(i8*, i32)    


declare external ccc  i32 @chdir(i8*)    


declare external ccc  i32 @rmdir(i8*)    


declare external ccc  i32 @sleep(i32)    


declare external ccc  i32 @usleep(i32)    


@PORT =    global i32 8001


@BUFFER_SIZE =    global i32 1024


@AF_INET =    global i32 2


@_0 = private unnamed_addr  constant [7 x i8] c"malloc\00", align 1


@_1 = private unnamed_addr  constant [7 x i8] c"socket\00", align 1


@_2 = private unnamed_addr  constant [5 x i8] c"bind\00", align 1


@_3 = private unnamed_addr  constant [7 x i8] c"listen\00", align 1


@_4 = private unnamed_addr  constant [22 x i8] c"Listening on port %d\0a\00", align 1


@_5 = private unnamed_addr  constant [7 x i8] c"accept\00", align 1


@_6 = private unnamed_addr  constant [21 x i8] c"Connection accepted\0a\00", align 1


@_7 = private unnamed_addr  constant [5 x i8] c"recv\00", align 1


@_8 = private unnamed_addr  constant [21 x i8] c"Message Received: %s\00", align 1


@_9 = private unnamed_addr  constant [3 x i8] c"%c\00", align 1


define external ccc  i32 @main()    {
; <label>:0:
  %1 = alloca i32 
  %2 = alloca i32 
  %3 = alloca i32 
  %4 = alloca i32 
  %5 = alloca i32 
  %6 = alloca {i16, i16, {i32}, i8*} 
  %7 = alloca i8* 
  %8 = alloca i32 
  %9 = alloca i32 
  store   i32 16, i32* %1  
  %10 = load   i32, i32* %1  
  store   i32 0, i32* %2  
  %11 = load   i32, i32* %2  
  store   i32 1, i32* %3  
  %12 = load   i32, i32* %3  
  store   i32 0, i32* %4  
  %13 = load   i32, i32* %4  
  store   i32 0, i32* %5  
  %14 = load   i32, i32* %5  
  %15 = load   {i16, i16, {i32}, i8*}, {i16, i16, {i32}, i8*}* %6  
  %16 = load   i32, i32* @BUFFER_SIZE  
  %17 =  call ccc  i8*  @malloc(i32  %16)  
  store   i8* %17, i8** %7  
  %18 = load   i8*, i8** %7  
  %19 = load   i8*, i8** %7  
  %20 = icmp eq i8* %19, zeroinitializer 
  %21 = icmp ne i1 %20, 0 
  br i1 %21, label %if.then_0, label %if.else_0 
if.then_0:
   call ccc  void  @perror(i8*  getelementptr inbounds ([7 x i8], [7 x i8]* @_0, i64 0, i64 0))  
  ret i32 1 
if.else_0:
  br label %if.merge_0 
if.merge_0:
  %22 = load   i32, i32* @AF_INET  
  %23 = load   i32, i32* %3  
  %24 =  call ccc  i32  @socket(i32  %22, i32  %23, i32  0)  
  store   i32 %24, i32* %4  
  %25 = load   i32, i32* %4  
  %26 = icmp slt i32 %25, 0 
  %27 = icmp ne i1 %26, 0 
  br i1 %27, label %if.then_1, label %if.else_1 
if.then_1:
   call ccc  void  @perror(i8*  getelementptr inbounds ([7 x i8], [7 x i8]* @_1, i64 0, i64 0))  
  ret i32 1 
if.else_1:
  br label %if.merge_1 
if.merge_1:
  %28 = load   i32, i32* @AF_INET  
  %29 = trunc i32 %28 to i16  
  %30 = getelementptr  {i16, i16, {i32}, i8*}, {i16, i16, {i32}, i8*}* %6, i32 0, i32 0 
  store   i16 %29, i16* %30  
  %31 = load   i32, i32* @PORT  
  %32 =  call ccc  i16  @htons(i32  %31)  
  %33 = getelementptr  {i16, i16, {i32}, i8*}, {i16, i16, {i32}, i8*}* %6, i32 0, i32 1 
  store   i16 %32, i16* %33  
  %34 = load   i32, i32* %2  
  %35 = getelementptr  {i16, i16, {i32}, i8*}, {i16, i16, {i32}, i8*}* %6, i32 0, i32 2 
  %36 = getelementptr  {i32}, {i32}* %35, i32 0, i32 0 
  store   i32 %34, i32* %36  
  %37 = load   i32, i32* %4  
  %38 = load   {i16, i16, {i32}, i8*}, {i16, i16, {i32}, i8*}* %6  
  %39 = alloca {i16, i16, {i32}, i8*} 
  store   {i16, i16, {i32}, i8*} %38, {i16, i16, {i32}, i8*}* %39  
  %40 = load   i32, i32* %1  
  %41 =  call ccc  i32  @bind(i32  %37, {i16, i16, {i32}, i8*}*  %39, i32  %40)  
  %42 = icmp slt i32 %41, 0 
  %43 = icmp ne i1 %42, 0 
  br i1 %43, label %if.then_2, label %if.else_2 
if.then_2:
   call ccc  void  @perror(i8*  getelementptr inbounds ([5 x i8], [5 x i8]* @_2, i64 0, i64 0))  
  ret i32 1 
if.else_2:
  br label %if.merge_2 
if.merge_2:
  %44 = load   i32, i32* %4  
  %45 =  call ccc  i32  @listen(i32  %44, i32  3)  
  %46 = icmp slt i32 %45, 0 
  %47 = icmp ne i1 %46, 0 
  br i1 %47, label %if.then_3, label %if.else_3 
if.then_3:
   call ccc  void  @perror(i8*  getelementptr inbounds ([7 x i8], [7 x i8]* @_3, i64 0, i64 0))  
  ret i32 1 
if.else_3:
  br label %if.merge_3 
if.merge_3:
  %48 = load   i32, i32* @PORT  
  %49 =  call ccc  i32 (i8*, ...) @printf(i8*  getelementptr inbounds ([22 x i8], [22 x i8]* @_4, i64 0, i64 0), i32  %48)  
  %50 = load   i32, i32* %4  
  %51 = load   {i16, i16, {i32}, i8*}, {i16, i16, {i32}, i8*}* %6  
  %52 = alloca {i16, i16, {i32}, i8*} 
  store   {i16, i16, {i32}, i8*} %51, {i16, i16, {i32}, i8*}* %52  
  %53 = load   i32, i32* %1  
  %54 = alloca i32 
  store   i32 %53, i32* %54  
  %55 =  call ccc  i32  @accept(i32  %50, {i16, i16, {i32}, i8*}*  %52, i32*  %54)  
  store   i32 %55, i32* %5  
  %56 = load   i32, i32* %5  
  %57 = icmp slt i32 %56, 0 
  %58 = icmp ne i1 %57, 0 
  br i1 %58, label %if.then_4, label %if.else_4 
if.then_4:
   call ccc  void  @perror(i8*  getelementptr inbounds ([7 x i8], [7 x i8]* @_5, i64 0, i64 0))  
  ret i32 1 
if.else_4:
  br label %if.merge_4 
if.merge_4:
  %59 =  call ccc  i32 (i8*, ...) @printf(i8*  getelementptr inbounds ([21 x i8], [21 x i8]* @_6, i64 0, i64 0))  
  br label %while.cond_0 
while.cond_0:
  br i1 1, label %while.body_0, label %while.exit_0 
while.body_0:
  %60 = load   i32, i32* %5  
  %61 = load   i8*, i8** %7  
  %62 = load   i32, i32* @BUFFER_SIZE  
  %63 =  call ccc  i32  @recv(i32  %60, i8*  %61, i32  %62, i32  0)  
  store   i32 %63, i32* %8  
  %64 = load   i32, i32* %8  
  %65 = load   i32, i32* %8  
  %66 = icmp eq i32 %65, 0 
  %67 = icmp ne i1 %66, 0 
  br i1 %67, label %if.then_5, label %if.else_5 
if.then_5:
  br label %while.exit_0 
if.else_5:
  br label %if.merge_5 
if.merge_5:
  %68 = load   i32, i32* %8  
  %69 = icmp slt i32 %68, 0 
  %70 = icmp ne i1 %69, 0 
  br i1 %70, label %if.then_6, label %if.else_6 
if.then_6:
   call ccc  void  @perror(i8*  getelementptr inbounds ([5 x i8], [5 x i8]* @_7, i64 0, i64 0))  
  br label %while.exit_0 
if.else_6:
  br label %if.merge_6 
if.merge_6:
  %71 = load   i8*, i8** %7  
  %72 =  call ccc  i32 (i8*, ...) @printf(i8*  getelementptr inbounds ([21 x i8], [21 x i8]* @_8, i64 0, i64 0), i8*  %71)  
  store   i32 0, i32* %9  
  %73 = load   i32, i32* %9  
  br label %for.cond_0 
for.cond_0:
  %74 = load   i32, i32* %9  
  %75 = load   i32, i32* %8  
  %76 = icmp slt i32 %74, %75 
  br i1 %76, label %for.body_0, label %for.exit_0 
for.body_0:
  %77 = load   i8*, i8** %7  
  %78 = load   i32, i32* %9  
  %79 = getelementptr  i8, i8* %77, i32 %78 
  %80 =  call ccc  i32 (i8*, ...) @printf(i8*  getelementptr inbounds ([3 x i8], [3 x i8]* @_9, i64 0, i64 0), i8*  %79)  
  br label %for.step_0 
for.step_0:
  %81 = load   i32, i32* %9  
  %82 = add   i32 %81, 1 
  store   i32 %82, i32* %9  
  br label %for.cond_0 
for.exit_0:
  %83 = load   i32, i32* %5  
  %84 = load   i8*, i8** %7  
  %85 = load   i32, i32* %8  
  %86 =  call ccc  i32  @send(i32  %83, i8*  %84, i32  %85, i32  0)  
  br label %while.cond_0 
while.exit_0:
  %87 = load   i32, i32* %5  
  %88 = icmp sgt i32 %87, 0 
  %89 = icmp ne i1 %88, 0 
  br i1 %89, label %if.then_7, label %if.else_7 
if.then_7:
  %90 = load   i32, i32* %5  
  %91 =  call ccc  i32  @close(i32  %90)  
  br label %if.merge_7 
if.else_7:
  br label %if.merge_7 
if.merge_7:
  %92 = phi i32 [%91, %if.then_7], [undef, %if.else_7] 
  %93 = load   i8*, i8** %7  
   call ccc  void  @free(i8*  %93)  
  ret i32 0 
}