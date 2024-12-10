# CC(x, C, i) =
#   1, x == 0
#   0, x < 0 or i > |C|
#   CC(x - Ci, C, i) + CC(x, C, i + 1)
def count_change(x, coins, index = 0)
    return 1 if x == 0
    return 0 if x < 0 || index >= coins.size
    count_change(x - coins[index], coins, index) + count_change(x, coins, index + 1)
end

# # Test:
# puts "Test #1: #{count_change(5, [1,2,5]) == 4? "Passed" : "Failed"}"
# puts "Test #2: #{count_change(100, [1,5,10,25,50]) == 292? "Passed" : "Failed"}"

# input: <x> <c1,c2,...,cn>
if ARGV.size < 2
    puts "Usage: ruby main.rb <x> <c1,c2,...,cn>"
    exit
end

x = ARGV[0].to_i
coins = ARGV[1].split(',').map(&:to_i)
result = count_change(x, coins)
puts "Number of ways to change #{x} with coins #{coins}: #{result}"
