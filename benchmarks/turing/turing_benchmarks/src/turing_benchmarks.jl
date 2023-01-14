module turing_benchmarks

using Turing
using BenchmarkTools
using DataFrames
using CSV
using Statistics

##### LDA

function wordsToIdxs(vocab, words)
  n_words = length(words)
  word_idxs = Vector{Int64}(undef, n_words)
  for i in 1:n_words
    word_idxs[i] = findfirst(w -> w==words[i], vocab)
  end
  return word_idxs
end

function idxsToWords(vocab, word_idxs)
  n_words = length(word_idxs)
  words = Vector{String}(undef, n_words)
  for i in 1:n_words
    words[i] = vocab[word_idxs[i]]
  end
  return word_idxs
end

@model function topicModel(doc_topic_ps, topic_word_ps, vocab, n_topics, n_words, word_idxs)
  # simulation
  if word_idxs === missing
    # initialise list of words observed
    words     = Vector{String}(undef, n_words)
    # initialise list of corresponding word indexes observed
    word_idxs = Vector{Int64}(undef, n_words)
  # inference
  else
    #  set length of words
    n_words   = length(word_idxs)
    # initialise list of words observed
    words     = Vector{String}(undef, n_words)
    # set list of words observed
    for i in 1:n_words
      words[i] = vocab[word_idxs[i]]
    end
  end

  # print(word_idxs)
  if topic_word_ps === missing
    # initialise list of word probabilities for each topic
    topic_word_ps = Vector{Vector{Float64}}(undef, n_topics)
  end

  # set list of topic probabilities for the document
  doc_topic_ps ~ Dirichlet(ones(n_topics))
  for i in 1:n_topics
    # set list of word probabilities for each topic
    topic_word_ps[i] ~ Dirichlet((ones(length(vocab))))
  end

  # initialise list of topics observed
  topic_obs = Vector{Int64}(undef, n_words)

  for i in 1:n_words
    # observe a topic
    topic_obs[i] ~ Categorical(doc_topic_ps)
    # fetch the topic's corresponding word distribution
    word_ps = topic_word_ps[topic_obs[i]]
    # observe a word index for that topic
    word_idxs[i] ~ Categorical(word_ps)
  end
  # print(word_idxs)

  # print(word_idxs)
  # print(doc_topic_ps)
  # print(topic_word_ps)
  return word_idxs
end

vocab     = ["DNA", "evolution", "parsing", "phonology"]
words     = ["DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA"]
word_idxs = wordsToIdxs(vocab, words)

##### Simulation

function topicSim(n_words)
  return topicModel(missing, missing, vocab, 2, n_words, missing)
end

##### Inference
function topicInfer(n_words)
  simModel = topicSim(n_words)
  ys = simModel()
  return topicModel(missing, missing, vocab, 2, n_words, ys)
end

##### LinRegr

# Define a simple Normal model with unknown mean and variance.
@model function linRegr(mu, c, σ, xs, ys)
  N = length(xs)
  if ys === missing
      # Initialize `x` if missing
      ys = Vector{Float64}(undef, N)
  end
  mu ~ Normal(0, 3)
  c  ~ Normal(0, 5)
  σ  ~ Uniform(1, 3)
  for n in 1:N
    ys[n] ~  Normal(mu  * xs[n] + c, σ)
  end
  return ys
end

##### Simulation

function linRegrSim(n_datapoints)
  xs = range(0.0, n_datapoints, length=n_datapoints)
  return linRegr(missing, missing, missing, xs, missing)
end

##### Inference

function linRegrInf(n_datapoints)
  simModel = linRegrSim(n_datapoints)
  ys = simModel()
  xs = range(0.0, n_datapoints, length=n_datapoints)
  return linRegr(missing, missing, missing, xs, ys)
end

##### HMM


@model function hmm(trans_p, obs_p, N, xs, ys)
  if ys === missing
      # Initialize `x` if missing
      ys = Vector{Int64}(undef, N)
      ys[1] = 0.0
  end
  if xs === missing
    # Initialize `x` if missing
      xs    = Vector{Int64}(undef, N)
      xs[1] = 0
  end
  trans_p ~ Uniform(0, 1)
  obs_p   ~ Uniform(0, 1)
  # print("trans_p: ", trans_p)
  # print("obs_p: ", obs_p)
  for n in 2:N
    dX    ~ Bernoulli(trans_p)
    # print("dX: ", dX)
    xs[n] = xs[n-1] + dX
    ys[n] ~ Binomial(xs[n], obs_p)
    # print("y: ", ys[n])
  end
  return ys
end

##### Simulation

function hmmSim(n_nodes)
  return hmm(missing, missing, n_nodes, missing, missing)
end

##### Inference

function hmmInf(n_nodes)
  simModel = hmmSim(n_nodes)
  ys = simModel()
  return hmm(missing, missing, n_nodes, missing, ys)
end

##### Main

benchmarkfile = "../benchmark-results.csv"

function parseBenchmark(label, b)
  df = DataFrame(Name = label, Mean = mean(b.times)/(1000000000))
  CSV.write(benchmarkfile, df, append=true)
end

function setupfile()
  CSV.write(benchmarkfile,[], writeheader=true, header=["Name", "Mean"])
end

macro benchmarkSampleSize(name, src, method, samplesizes)
  exprs = [
    quote
      path = string($name, "/sample-size/", $samplesize)
      print(path, '\n')
      trace = @benchmark sample($src(100), $method(), $samplesize)
      print("...complete\n")
      parseBenchmark(path, trace)
    end
  for samplesize in eval(samplesizes)]
  return Expr(:block, exprs...)
end

macro benchmarkDataSize(name, src, method, datasizes)
  exprs = [
    quote
      path = string($name, "/data-size/", $datasize)
      print(path, '\n')
      trace = @benchmark sample($src($datasize), $method(), 2000)
      print("...complete\n")
      parseBenchmark(path, trace)
    end
  for datasize in eval(datasizes)]
  return Expr(:block, exprs...)
end

macro samplesizes() return :([2000, 4000, 6000, 8000, 10000]) end
macro largedatasizes() return :([200, 400, 600, 800, 1000]) end
macro smalldatasizes() return :([40, 80, 120, 160, 200]) end


function main()
  setupfile()

  # LinRegr
  @benchmarkSampleSize("linRegr/Sim", linRegrSim, Prior, @samplesizes)
  @benchmarkSampleSize("linRegr/LW", linRegrInf, IS, @samplesizes)
  @benchmarkSampleSize("linRegr/MH", linRegrInf, MH, @samplesizes)
  @benchmarkDataSize("linRegr/Sim", linRegrSim, Prior, @largedatasizes)
  @benchmarkDataSize("linRegr/LW", linRegrInf, IS, @largedatasizes)
  @benchmarkDataSize("linRegr/MH", linRegrInf, MH, @largedatasizes)

  # LDA
  @benchmarkSampleSize("lda/Sim", topicSim, Prior, @samplesizes)
  @benchmarkSampleSize("lda/LW", topicInfer, IS, @samplesizes)
  @benchmarkSampleSize("lda/MH", topicInfer, MH, @samplesizes)
  @benchmarkDataSize("lda/Sim", topicSim, Prior, @smalldatasizes)
  @benchmarkDataSize("lda/LW", topicInfer, IS, @smalldatasizes)
  @benchmarkDataSize("lda/MH", topicInfer, MH, @smalldatasizes)

  #HMM
  @benchmarkSampleSize("hmm/Sim", hmmSim, Prior, @samplesizes)
  @benchmarkSampleSize("hmm/LW", hmmInf, IS, @samplesizes)
  @benchmarkSampleSize("hmm/MH", hmmSim, MH, @samplesizes)
  @benchmarkDataSize("hmm/Sim", hmmSim, Prior, @smalldatasizes)
  @benchmarkDataSize("hmm/LW", hmmInf, IS, @smalldatasizes)
  @benchmarkDataSize("hmm/MH", hmmInf, MH, @smalldatasizes)

end

main()

end
