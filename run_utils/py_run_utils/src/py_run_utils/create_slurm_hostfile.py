import os
import re
import argparse
import math



def expand(nodelist):

    if '[' not in nodelist:
        raise ValueError("Invalid nodelist format: missing opening '[' in '{nodelist}'!")

    tmp = re.split('\[', nodelist)
    base_string = tmp[0]

    if tmp[1][-1] != ']':
        raise ValueError("Invalid nodelist format: missing closing ']' in '{nodelist}'!")

    main_string = tmp[1][:-1]

    nodes = []
    for node_range in re.split(',', main_string):
        range_bounds = re.split('-', node_range)

        if len(range_bounds) == 1:
            nodes.append(base_string+range_bounds[0])
        else:
            id_length = len(range_bounds[0])

            for i in range(int(range_bounds[0]), int(range_bounds[1])+1):
                nodes.append(base_string+str(i).zfill(id_length))

    return nodes


def create_slurm_hostfile_load_balanced():

    parser = argparse.ArgumentParser(description="Create the SLURM hostfile for a custom task distribution ensuring load balancing across nodes")

    parser.add_argument("--output_filepath",         type=str, required=True,               help="Path to the output SLURM hostfile.")
    parser.add_argument("--tot_tasks_per_node",      type=int, required=True,               help="The desired total number of tasks per node.")
    parser.add_argument("--atm_comp_tasks_per_node", type=int, required=True,               help="The number of compute tasks per node for the atmosphere.")
    parser.add_argument("--atm_io_tasks",            type=int, required=True,               help="The total number of IO tasks for the atmosphere.")
    parser.add_argument("--oce_io_tasks",            type=int, required=True,               help="The total number of IO tasks for the ocean.")
    parser.add_argument("--threads_per_task",        type=int, required=True,               help="The number of threads for each task.")
    parser.add_argument("--max_threads_per_node",    type=int, required=False, default=288, help="Maximum number of threads allowed per node.")

    args = parser.parse_args()

    nodes = expand(os.environ['SLURM_JOB_NODELIST'])
    number_of_nodes = len(nodes)

    output_filepath = args.output_filepath

    max_threads_per_node = args.max_threads_per_node

    tot_tasks_per_node      = args.tot_tasks_per_node
    atm_comp_tasks_per_node = args.atm_comp_tasks_per_node
    atm_io_tasks            = args.atm_io_tasks
    oce_io_tasks            = args.oce_io_tasks
    threads_per_task        = args.threads_per_task

    # ATTENTION:
    # Do not remove the following checks on IO tasks if you don't know what you
    # are doing, you might mess up the whole task distribution logic and 
    # negatively impact performance
    if atm_io_tasks > number_of_nodes:
        raise ValueError(f"The total number of IO tasks for the atmosphere ({atm_io_tasks}) exceeds the number of nodes ({number_of_nodes}). "
                          "Only 1 atmosphere IO task per node is allowed. "
                          "Stopping execution!")

    if oce_io_tasks > number_of_nodes:
        raise ValueError(f"The total number of IO tasks for the ocean ({oce_io_tasks}) exceeds the number of nodes ({number_of_nodes}). "
                          "Only 1 ocean IO task per node is allowed. "
                          "Stopping execution!")

    tot_threads_per_node = tot_tasks_per_node * threads_per_task

    if tot_threads_per_node > max_threads_per_node:
        raise ValueError(f"The desired number of threads per node ({tot_tasks_per_node} * {threads_per_task} (threads per task) = {tot_threads_per_node}) exceeds the allowed maximum ({max_threads_per_node}). "
                          "Stopping execution!")

    n_available_threads = {nid: tot_threads_per_node for nid in nodes}

    with open(output_filepath, 'w') as file:

        # Atmosphere compute procs (4 GPUs per node; generally all nodes are filled)
        # Equal to SLURM's --distribution="plane=atm_comp_tasks_per_node"
        for nid in nodes:
            file.write((nid + '\n') * atm_comp_tasks_per_node)

            n_available_threads[nid] -= atm_comp_tasks_per_node * threads_per_task

        # Atmosphere IO procs
        # Equal to SLURM's --distribution="cyclic"
        for i in range(atm_io_tasks):
            nid = nodes[i % number_of_nodes]
            file.write(nid + '\n')

            n_available_threads[nid] -= 1 * threads_per_task

        # Dry-run for ocean IO procs (to make sure we fill up to
        # tot_tasks_per_node with ocean compute procs)
        for i in range(oce_io_tasks):
            nid = nodes[(atm_io_tasks + i) % number_of_nodes]

            n_available_threads[nid] -= 1 * threads_per_task

        # Ocean compute procs
        # Fill up to the number of still available cores
        for nid in nodes:
            n_remaining_tasks = (n_available_threads[nid] // threads_per_task)
            file.write((nid + '\n') * n_remaining_tasks)

            n_available_threads[nid] -= n_remaining_tasks

        # Ocean IO procs (avoid overlap with atmosphere IO procs)
        # Equal to SLURM's --distribution="cyclic"
        for i in range(oce_io_tasks):
            nid = nodes[(atm_io_tasks + i) % number_of_nodes]
            file.write(nid + '\n')


def create_slurm_hostfile_separate_io():

    parser = argparse.ArgumentParser(description="Create the SLURM hostfile for a custom task distribution ensuring load balancing across nodes")

    parser.add_argument("--output_filepath",         type=str, required=True,               help="Path to the output SLURM hostfile.")
    parser.add_argument("--tot_tasks_per_comp_node", type=int, required=True,               help="The desired total number of (compute) tasks per (compute) node.")
    parser.add_argument("--max_tasks_per_io_node",   type=int, required=True,               help="The maximum number of (IO) tasks allowed per IO node.")
    parser.add_argument("--atm_comp_tasks_per_node", type=int, required=True,               help="The number of compute tasks per node for the atmosphere.")
    parser.add_argument("--atm_io_tasks",            type=int, required=True,               help="The total number of IO tasks for the atmosphere.")
    parser.add_argument("--oce_io_tasks",            type=int, required=True,               help="The total number of IO tasks for the ocean.")
    parser.add_argument("--threads_per_task",        type=int, required=True,               help="The number of threads for each task.")
    parser.add_argument("--max_threads_per_node",    type=int, required=False, default=288, help="Maximum number of threads allowed per node.")

    args = parser.parse_args()

    nodes = expand(os.environ['SLURM_JOB_NODELIST'])

    output_filepath = args.output_filepath

    max_threads_per_node = args.max_threads_per_node

    tot_tasks_per_comp_node = args.tot_tasks_per_comp_node
    max_tasks_per_io_node   = args.max_tasks_per_io_node
    atm_comp_tasks_per_node = args.atm_comp_tasks_per_node
    atm_io_tasks            = args.atm_io_tasks
    oce_io_tasks            = args.oce_io_tasks
    threads_per_task        = args.threads_per_task

    tot_threads_per_comp_node = tot_tasks_per_comp_node * threads_per_task
    max_threads_per_io_node = max_tasks_per_io_node * threads_per_task

    if tot_threads_per_comp_node > max_threads_per_node:
        raise ValueError(f"The desired number of threads per compute node ({tot_tasks_per_comp_node} * {threads_per_task} (threads per task) = {tot_threads_per_comp_node}) exceeds the allowed maximum ({max_threads_per_node}). "
                          "Stopping execution!")

    if max_threads_per_io_node > max_threads_per_node:
        raise ValueError(f"The maximum desired number of threads per IO node ({max_tasks_per_io_node} * {threads_per_task} (threads per task) = {max_threads_per_io_node}) exceeds the allowed maximum ({max_threads_per_node}). "
                          "Stopping execution!")

    tot_io_tasks = atm_io_tasks + oce_io_tasks
    tot_io_nodes = math.ceil(tot_io_tasks / max_tasks_per_io_node)

    compute_nodes = nodes[:-tot_io_nodes]
    io_nodes = nodes[-tot_io_nodes:]

    n_available_threads = {nid: tot_threads_per_node for nid in nodes}

    with open(output_filepath, 'w') as file:

        # Atmosphere compute procs (4 GPUs per compute node; generally all compute nodes are filled)
        # Equal to SLURM's --distribution="plane=atm_comp_tasks_per_node" over compute nodes
        for nid in compute_nodes:
            file.write((nid + '\n') * atm_comp_tasks_per_node)

            n_available_threads[nid] -= atm_comp_tasks_per_node * threads_per_task

        # Atmosphere IO procs
        # Equal to SLURM's --distribution="plane=max_tasks_per_io_node", but less tasks on the last
        # node if atm_io_tasks is not divisible by max_tasks_per_io_node
        for i in range(atm_io_tasks):
            nid = io_nodes[i // max_tasks_per_io_node]
            file.write(nid + '\n')

            n_available_threads[nid] -= 1 * threads_per_task

        # Ocean compute procs
        # Fill up to the number of still available cores
        for nid in compute_nodes:
            n_remaining_tasks = (n_available_threads[nid] // threads_per_task)
            file.write((nid + '\n') * n_remaining_tasks)

            n_available_threads[nid] -= n_remaining_tasks

        # Ocean IO procs (avoid overlap with atmosphere IO procs)
        # Equal to SLURM's --distribution="plane=max_tasks_per_io_node", but less tasks on the first
        # node if atm_io_tasks is not divisible by max_tasks_per_io_node and less tasks on the last
        # node if oce_io_tasks is not divisible by max_tasks_per_io_node
        for i in range(oce_io_tasks):
            nid = io_nodes[(atm_io_tasks + i) // max_tasks_per_io_node]
            file.write(nid + '\n')

            n_available_threads[nid] -= 1 * threads_per_task
