#!/usr/bin/ruby

require 'set'

def isect(a, b) 
  a = a.to_set
  o = Array.new

  b.each do |i|
    if a.include?(i)
        o.push(i) 
    end
  end

  return o
end

x = Array.new
y = Array.new

n = gets.chomp.to_i  

n.times do 
    l = gets.chomp.split
    x.push(l[0])
    y.push(l[1])
end

t2 = Time.now
xy = isect(x, y)
t3 = Time.now
printf("Set   | n = %d : %d intersects found in %f seconds\n",x.size,xy.size,t3-t2)
