from qiskit import QuantumCircuit, transpile
from qiskit_aer import AerSimulator
from qiskit.visualization import plot_histogram
import matplotlib.pyplot as plt

# Create a quantum circuit with 2 qubits
qc = QuantumCircuit(2, 2)

# Add a Hadamard gate to the first qubit
qc.h(0)

# Add a CNOT gate
qc.cx(0, 1)

# Measure both qubits
qc.measure_all()

# Print the circuit
print(qc)

# Simulate the circuit
simulator = AerSimulator()
compiled_circuit = transpile(qc, simulator)
result = simulator.run(compiled_circuit, shots=1024).result()
counts = result.get_counts(compiled_circuit)

print(f"Results: {counts}")