library(torch)

t <- torch_tensor(c(1, 2))            # a 1d vector of length 2
t <- torch_tensor(c(TRUE, FALSE))    # also 1d, but of type boolean
t <- torch_tensor( rbind(c(1,2,0), 
                         c(3,0,0), 
                         c(4,5,6)  ) )   # a 3x3 tensor (matrix)
t

t <- torch::torch_tensor( matrix(1:9, ncol = 3, byrow = TRUE))   # also 3x3
t

# a 3x3 tensor of standard-normally distributed values
t <- torch_randn(3, 3)
t
# a 4x2x2 (3d) tensor of zeroes
t <- torch_zeros(4, 2, 2)
t
torch_tensor 
-2.1563  1.7085  0.5245
0.8955 -0.6854  0.2418
0.4193 -0.7742 -1.0399
[ CPUFloatType{3,3} ]
torch_tensor 
(1,.,.) = 
    0  0
0  0
(2,.,.) = 
    0  0
0  0
(3,.,.) = 
    0  0
0  0
(4,.,.) = 
    0  0
0  0
[ CPUFloatType{4,2,2} ]

# Many similar functions exist,  including, e.g.,
# torch_arange() to create a tensor holding a sequence of evenly spaced values,
# torch_eye() which returns an identity matrix, and 
# torch_logspace() which fills a specified range with a list of values spaced logarithmically.


# torch tensors live on a device. By default, this will be the CPU:
t$device
torch_device(type='cpu')
# But we could also define a tensor to live on the GPU:
t <- torch_tensor(2, device = "cuda")
t$device
torch_device(type='cuda', index=0)

# another very important parameter to the tensor-creation functions: requires_grad

# To convert torch tensors to R, use as_array():

t <- torch_tensor(matrix(1:9, ncol = 3, byrow = TRUE))
as_array(t)
[,1] [,2] [,3]
[1,]    1    2    3
[2,]    4    5    6
[3,]    7    8    9

# Depending on whether the tensor is one-, two-, or three-dimensional, the resulting R object will be a vector, a matrix, or an array:
t <- torch_tensor(c(1, 2, 3))
as_array(t) %>% class()
t <- torch_ones(c(2, 2))
as_array(t) %>% class()
t <- torch_ones(c(2, 2, 2))
as_array(t) %>% class()

# If a tensor currently lives on the GPU, you need to move it to the CPU first:
t <- torch_tensor(2, device = "cuda")
as.integer(t$cpu())

# Indexing and slicing: the R-like part
t <- torch_tensor(rbind(c(1,2,3), c(4,5,6)))
t

t[1, 1]   # a single value
t[1, ]    # first row, all columns
t[1, 1:2] # first row, a subset of columns
torch_tensor 
1  2  3
4  5  6
[ CPUFloatType{2,3} ]
torch_tensor 
1
[ CPUFloatType{} ]
torch_tensor 
1
2
3
[ CPUFloatType{3} ]
torch_tensor 
1
2
[ CPUFloatType{2} ]