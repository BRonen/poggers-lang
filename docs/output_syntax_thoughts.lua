-- runtime errors message when currying functions
--[[
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
]]

-- recursion
--[[
    continuation is really complex to handle tail calls
    maybe goto solves most of the problems
    
    function tail_rec (a)
        return a <= 0 and "done" or tail_rec(a - 1)
    end
]]
function tail_rec (a)
    local some_hashing = a
    
    ::label_hashed::

    if (some_hashing <= 0) then return "done" else
        some_hashing = some_hashing - 1
        goto label_hashed
    end
end

print(tail_rec(1000000000))

--[[
$ time luajit output_syntax_thoughts.lua
done

real    0m5,881s
user    0m5,878s
sys     0m0,003s
]]