-- runtime errors message when currying functions
local wasd = function(a, callData)
    if(not a) then throw "Error at " .. callData end
    
    return function(b, callData2)
        if(not b) then throw "Insuficient parameters at " .. callData2 end

        return a + b
    end
end

-- implicit call metadata of applications
local c = wasd(1, "line 8")
local d = wasd(1, "line 9")