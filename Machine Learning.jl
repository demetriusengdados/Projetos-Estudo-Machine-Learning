model = Chain(
    Dense(2, 3, relu),
    Dense(3, 2),
    softmax
)

function printfacts()
    println("Neural Net is awesome")
end

printfacts()

for step in 1:1000
    i = rand(1:length(x))
    loss = Flux.mse(model(x[i]), y[i])
    back!(loss)
    model.W.data .-= model.W.grad * lr
    model.b.data .-= model.b.grad * lr
end

using Pkg
Pkg.add("Flux")
using Flux
using Flux.Tracker

x = [[0, 0], [0, 1],[1,0], [1, 1]]
y = [[0], [1], [1], [0]] 

model = Dense(2, 1, #simbolo do omega)
    lr = 0.1

#testando o modelo    
model(x[1])

model(x[2])
