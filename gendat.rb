#!/usr/bin/env ruby

n = 10000
#puts "[#{(0...n).map { |x| "{a: {b: #{x}}, c: 2}" }.join(",")}]"
puts "[#{(0...n).map { |x| "{\"a\": {\"b\": #{x}}, \"c\": 2}" }.join(",")}]"
