using DataFrames
using Tables
using StatsBase
using LinearAlgebra
using Random

import Base.Threads.@spawn

using JLD
using RData
import CodecXz

locally = false

if locally
    dat_dir = "/Users/loranknol/HPC_project/data/"
else
    dat_dir = "/project_cephfs/3022017.02/projects/lorkno/data"    
end

### Option: Permute the matrix ownership to generate a null distribution?
permute = false

### Option: Number of partitions used to calculate TDIA
# Takes about 1:30 hours with 5 cores using BLAS
# Takes bout 50 minutes with 10 cores using parallelisation
n_parts = 30 

### Option: How many iterations should be executed (in total)
if locally
    n_iter = 10
else
    n_iter = 10000
end

function subsampled_tdia(subject_mats, n_parts, permute::Bool)
    n_sub = length(subject_mats)

    # Because we want to sample entire blocks of days (currently max
    # 30 days), we have to calculate eligible starting indices of those blocks,
    # and then sample a single starting index per subject
    lens = length.(subject_mats)
    max_start_idx = @. lens - n_parts + 1

    # Sample a single starting index per subject
    sampled_start_idx = rand.(range.(1, max_start_idx))
    # Expand every start index to a block of indices
    sampled_idx = range.(sampled_start_idx, @. sampled_start_idx + n_parts - 1)

    # Subsampled matrices
    sub_mats = view.(subject_mats, sampled_idx)

    # Every (unique) pair of matrices represents an edge, and for every edge 
    # we need to compute a correlation

    # A coordinate pair [e.g. (i, j), which represents a subject's matrix] can also be represented 
    # as a single number, i.e. (i - 1) * n_parts + j. To create pairs of coordinate pairs 
    # (i.e. edges between all matrices of all subjects), we can create pairs of these single numbers instead.
    n_mats = n_sub * n_parts
    coords = 1:n_mats

    if permute
        # Don't permute within subjects, only permute the subjects themselves.
        # Preserves within-subjects correlation.
        permuted_idx_between = randperm(n_sub)

        # sub_mats is a vector of views, we have to be careful that we don't modify the
        # views themselves, or we'll accidentally change subject_mats.
        sub_mats = sub_mats[permuted_idx_between]
    end

    # Squash vector of vector of matrices to a [n_entries]X[n_sub*n_parts] matrix.
    # Faster and less memory-intensive than calling mapreduce twice.
    n_entries = length(subject_mats[1][1])
    mat = Matrix{Int32}(undef, n_entries, n_sub * n_parts)
    for i = 1:n_sub, j = 1:n_parts
        mat[:, (i - 1) * n_parts + j] = vec(sub_mats[i][j])
    end

    cors = cor(mat)

    # Set diagonal to -10 instead of 1, so we won't have to take into account the
    # correlation with the source matrix itself when assessing the highest cor-
    # relations
    cors[diagind(cors)] .= -10

    tdia = Matrix{Float64}(undef, n_sub, n_parts)

    # Confusion matrix
    conf_mat = zeros(Int32, n_sub, n_sub)

    for x = coords
        # Select the n_parts-1 highest correlations (the first one will no longer 
        # be the correlation of source with itself)
        match_idx = sortperm(cors[:, x], rev = true)[1:n_parts - 1]

        # Get subject index from x (calculate row number of the n_sub*n_parts matrix) 
        # and match_idx (also row number of that matrix)
        sub_idx = (x - 1) ÷ n_parts + 1
        match_idx = @. (match_idx - 1) ÷ n_parts + 1

        # Partition index
        part_idx = mod(x - 1, n_parts) + 1

        if permute
            # Get actual subject number; otherwise we'd be comparing the permuted
            # subject index to the permuted match index, which is like doing no
            # permutation at all.
            sub_idx = permuted_idx_between[sub_idx]
        end

        tdia[sub_idx, part_idx] = sum(match_idx .== sub_idx) / (n_parts - 1)

        for m = match_idx
            conf_mat[sub_idx, m] += Int32(1)
        end
    end

    # Return these values
    tdia, sampled_idx, conf_mat
end

function subsampled_tdia_replicate(subject_mats, n_parts, permute::Bool, n_iter)
    tdias = Array{Float64, 3}(undef, n_sub, n_parts, n_iter)
    sampled_idxs = Array{UnitRange{Int64}, 2}(undef, n_sub, n_iter)

    for i = 1:n_iter
        tdias[:, :, i], sampled_idxs[:, i] = subsampled_tdia(subject_mats, n_parts, permute)
    end
    
    tdias, sampled_idxs
end

function mainp(subject_mats::Vector{Vector{Matrix{Int32}}}, permute, n_parts, n_iter)
    n_sub = length(subject_mats)

    tdias = Array{Float64, 3}(undef, n_sub, n_parts, n_iter)
    sampled_idx = Array{UnitRange{Int64}, 2}(undef, n_sub, n_iter)
    conf_mats = Array{Int32, 3}(undef, n_sub, n_sub, n_iter)

    Threads.@threads for i = 1:n_iter
        tdias[:, :, i], sampled_idx[:, i], conf_mats[:, :, i] = subsampled_tdia(subject_mats, n_parts, permute)
    end

    # n_threads = Threads.nthreads()
    # threads = Array{Task, 1}(undef, n_threads)
    # for i = 1:n_threads
    #     threads[i] = Threads.@spawn subsampled_tdia_replicate(subject_mats, n_parts, false, n_iter ÷ n_threads)
    # end

    # @time tdias = mapreduce(fetch, vcat, threads)

    sampled_idx = flat(Vector{Int32}.(sampled_idx), n_sub, n_iter, n_parts)
    # Using reduce(+, ...) instead of sum(...) preserves the Int32 type,
    # which might be safer when interfacing with R.
    conf_mat = dropdims(reduce(+, conf_mats, dims=3), dims=3)

    tdias, sampled_idx, conf_mat
end

function main(subject_mats::Vector{Vector{Matrix{Int32}}}, permute, n_parts, n_iter)
    n_sub = length(subject_mats)

    tdias = Array{Float64, 3}(undef, n_sub, n_parts, n_iter)
    sampled_idx = Array{UnitRange{Int64}, 2}(undef, n_sub, n_iter)
    conf_mats = Array{Int32, 3}(undef, n_sub, n_sub, n_iter)

    for i = 1:n_iter
        if mod(i, 1000) == 0
            println(i)
        end

        tdias[:, :, i], sampled_idx[:, i], conf_mats[:, :, i] = subsampled_tdia(subject_mats, n_parts, permute)
    end

    sampled_idx = flat(Vector{Int32}.(sampled_idx), n_sub, n_iter, n_parts)
    # Using reduce(+, ...) instead of sum(...) preserves the Int32 type,
    # which might be safer when interfacing with R.
    conf_mat = dropdims(reduce(+, conf_mats, dims=3), dims=3)

    tdias, sampled_idx, conf_mat
end

function flat(sampled_idx::Matrix{Vector{Int32}}, n_sub, n_iter, n_parts)
    flattened = Array{Int32, 2}(undef, n_sub * n_iter, n_parts)

    for i = 1:(n_sub * n_iter), j = 1:n_parts
        flattened[i, j] = sampled_idx[(i - 1) ÷ n_iter + 1, mod(i - 1, n_iter) + 1][j]
    end

    flattened
end

objs = load(joinpath(dat_dir, "bound_mats.rda"))
subject_mats = objs["bound_mats"];

tdias, sampled_idx, conf_mat = mainp(subject_mats, permute, n_parts, n_iter)

if !locally
    save(joinpath(dat_dir, "tdias_bound_hists.jld"), 
        "tdias", tdias, 
        "sampled_idx", sampled_idx,
        "conf_mat", conf_mat,
        compress = true, compatible = true)
end