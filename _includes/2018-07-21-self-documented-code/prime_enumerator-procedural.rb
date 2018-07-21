# Yields ordered prime numbers up to infinity
# It uses "Sieve of Eratosthenes" algorithm to find all primes less than 8192
# After it reached it rebuild sieve with limit increased twice
# It takes only O(n**2) to take first n primes
class PrimeEnumerator
  # @return [Enumerator] enumeratior of prime numbers
  def self.enumerate!
    Enumerator.new do |x|
      instance = new
      loop { x << instance.next }
    end
  end

  # We have following instance variables:
  # @primes is an array of primes, which extends automatically when limit
  #   reached. We need to store them to evaluate next primes
  # @limit is not less than maximum prime number in @primes
  #   and less then next prime
  # @idx is next enumeration index in @primes
  def initialize
    @idx = 0
    @limit = 1
    @primes = []
  end

  # Returns next prime number and increments counter
  def next
    increase_limit! while @idx >= @primes.length
    @idx += 1
    @primes[@idx - 1]
  end

  # Increase @limit when all primes <= @limit already enumerated
  def increase_limit!
    lb = @limit + 1 # left bracket
    rb = @limit *= 2 # right bracket

    sieve = build_sieve(lb, rb)
    collect_primes_from_sieve(sieve, lb)
  end

  # This array of booleans represents (lb..rb) part of Eratosthenes sieve
  # By defenition, Eratosthenes sieve is array where all multiplicators
  # of found are marked as "not prime" or false. We index it from 0
  # to rb - lb so wee need to add lb to all primes
  def build_sieve(lb, rb)
    @primes.each_with_object(Array.new(rb - lb + 1) { true }) do |prime, sieve|
      j = (lb - 1) / prime
      sieve[prime * j - lb] = false while prime * (j += 1) < rb
    end
  end

  # No one element of (n+1..2n) can be multiplicator of other so all
  # indexes left true are primes
  def collect_primes_from_sieve(sieve, lb)
    sieve.each_with_index do |is_prime, idx|
      # build_sieve return value is indexed from 0 instead from lb
      @primes << idx + lb if is_prime
    end
  end
end
