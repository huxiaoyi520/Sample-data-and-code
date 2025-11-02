######################简单交互####################################################################################################################
import numpy as np
import matplotlib.pyplot as plt
from sklearn.naive_bayes import GaussianNB
from sklearn.svm import SVC
from matplotlib.backends.backend_pdf import PdfPages
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis as LDA
from sklearn.discriminant_analysis import QuadraticDiscriminantAnalysis as QDA
from sklearn.neural_network import MLPClassifier
from matplotlib.colors import ListedColormap
# Set global font to Arial Bold, size 12
plt.rcParams.update({
    'font.family': 'Arial',
    'font.weight': 'bold',
    'font.size': 6
})

# Function to generate data for specific settings
def generate_data(mean1, cov1, mean2, cov2, n_samples=500):
    np.random.seed(42)
    # Red class: rotate clockwise 45 degrees
    rotation_matrix_red = [[np.cos(-np.pi / 4), -np.sin(-np.pi / 4)], [np.sin(-np.pi / 4), np.cos(-np.pi / 4)]]
    cov1_rotated = np.dot(rotation_matrix_red, np.dot(cov1, np.transpose(rotation_matrix_red)))
    red_data = np.random.multivariate_normal(mean1, cov1_rotated, n_samples)

    # Blue class: rotate counter-clockwise 30 degrees
    rotation_matrix_blue = [[np.cos(np.pi / 6), -np.sin(np.pi / 6)], [np.sin(np.pi / 6), np.cos(np.pi / 6)]]
    cov2_rotated = np.dot(rotation_matrix_blue, np.dot(cov2, np.transpose(rotation_matrix_blue)))
    blue_data = np.random.multivariate_normal(mean2, cov2_rotated, n_samples)

    return red_data, blue_data

# Data configurations
original_mean1 = [3, 3]
original_cov1 = [[0.2, 0], [0, 0.5]]
original_mean2 = [0, 0]
original_cov2 = [[0.3, 0], [0, 0.6]]

# Generate datasets
red_original, blue_original = generate_data(original_mean1, original_cov1, original_mean2, original_cov2)
red_zero_mean, blue_zero_mean = generate_data([0, 0], original_cov1, [0, 0], original_cov2)
red_zero_cov, blue_zero_cov = generate_data([0, 0], [[1, 0], [0, 1]], [0, 0], [[1, 0], [0, 1]])

# Custom color map: Light Blue and Pink
cmap = ListedColormap(['lightblue', 'pink'])

# Prepare data
def prepare_data(red_data, blue_data):
    X = np.vstack((red_data, blue_data))
    y = np.array([1] * len(red_data) + [0] * len(blue_data))
    return X, y

X_original, y_original = prepare_data(red_original, blue_original)
X_zero_mean, y_zero_mean = prepare_data(red_zero_mean, blue_zero_mean)
X_zero_cov, y_zero_cov = prepare_data(red_zero_cov, blue_zero_cov)

# Classifiers
classifiers = [
    LDA(),
    GaussianNB(),
    QDA(),
    MLPClassifier(hidden_layer_sizes=(10,), max_iter=1000, random_state=42),
    SVC(kernel="linear", probability=True)
]
titles = ["LDA", "NB", "QDA", "NN", "gSVM"]

# Plot decision boundaries
def plot_decision_boundaries_no_borders(ax, X, y, classifiers, titles):
    xx, yy = np.meshgrid(
        np.linspace(X[:, 0].min() - 1, X[:, 0].max() + 1, 200),
        np.linspace(X[:, 1].min() - 1, X[:, 1].max() + 1, 200)
    )
    for i, (clf, title) in enumerate(zip(classifiers, titles)):
        clf.fit(X, y)
        Z = clf.predict(np.c_[xx.ravel(), yy.ravel()]).reshape(xx.shape)
        acc = clf.score(X, y)
        ax[i].contourf(xx, yy, Z, alpha=0.5, cmap=cmap)
        ax[i].scatter(X[y == 1, 0], X[y == 1, 1], color="red", alpha=0.6, s=0.5,edgecolors=None)  # Red points
        ax[i].scatter(X[y == 0, 0], X[y == 0, 1], color="blue", alpha=0.6, s=0.5,edgecolors=None)  # Blue points
        ax[i].set_title(f"{title}\nacc: {acc:.2f}", fontsize=6, fontweight='bold', fontname='Arial')
        ax[i].set_xlim(X[:, 0].min() - 1, X[:, 0].max() + 1)  # Auto scale x-axis
        ax[i].set_ylim(X[:, 1].min() - 1, X[:, 1].max() + 1)  # Auto scale y-axis
        ax[i].axis('off')  # Turn off the axis completely

# Save the plots to a resizable PDF file
pdf_filename = "simple_patterns_adjustable.pdf"
# # Convert mm to inches for figsize (1 inch = 25.4 mm)
width_mm = 100
height_mm = 90
width_in = width_mm / 25.4
height_in = height_mm / 25.4


with PdfPages(pdf_filename) as pdf:
    fig, axes = plt.subplots(3, 5, figsize=(width_in, height_in))
    fig.suptitle("Simple Patterns", fontsize=6, fontweight='bold', fontname='Arial')

    # Original data
    plot_decision_boundaries_no_borders(axes[0], X_original, y_original, classifiers, titles)

    # Zero mean
    plot_decision_boundaries_no_borders(axes[1], X_zero_mean, y_zero_mean, classifiers, titles)

    # Zero mean, covariance 1
    plot_decision_boundaries_no_borders(axes[2], X_zero_cov, y_zero_cov, classifiers, titles)

    plt.tight_layout(rect=[0, 0, 1, 0.96])
    pdf.savefig(fig)  # Save the figure to the PDF
    plt.close(fig)

print(f"PDF file saved as {pdf_filename}")






###########################复杂交互################################################################################################################################
import numpy as np
import matplotlib.pyplot as plt
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis as LDA
from sklearn.naive_bayes import GaussianNB
from sklearn.discriminant_analysis import QuadraticDiscriminantAnalysis as QDA
from sklearn.neural_network import MLPClassifier
from matplotlib.backends.backend_pdf import PdfPages
from sklearn.svm import SVC
from matplotlib.colors import ListedColormap

# Define classifiers
models = {
    'LDA': LDA(),
    'NB': GaussianNB(),
    'QDA': QDA(),
    'NN': MLPClassifier(max_iter=1000, random_state=42),
    'gSVM': SVC(kernel='rbf', probability=True, random_state=42)
}

# Generate data for the first row
def generate_first_row_data():
    np.random.seed(42)
    variance = 0.5
    # Red class (3 Gaussians)
    red_data = np.vstack([
        np.random.multivariate_normal([6, 6], np.eye(2) * variance, 200),
        np.random.multivariate_normal([6, 2], np.eye(2) * variance, 150),
        np.random.multivariate_normal([2, 6], np.eye(2) * variance, 150)
    ])
    # Blue class (4 Gaussians, rotated 45°)
    rotation_matrix = [[np.cos(np.pi / 4), -np.sin(np.pi / 4)], [np.sin(np.pi / 4), np.cos(np.pi / 4)]]
    blue_data = np.vstack([
        np.random.multivariate_normal([-6, -6], np.eye(2) * variance, 125),
        np.random.multivariate_normal([-3, -6], np.eye(2) * variance, 125),
        np.random.multivariate_normal([-6, -3], np.eye(2) * variance, 125),
        np.random.multivariate_normal([-3, -3], np.eye(2) * variance, 125)
    ])
    blue_data = np.dot(blue_data, rotation_matrix)
    X = np.vstack([red_data, blue_data])
    y = np.array([1] * len(red_data) + [0] * len(blue_data))
    return X, y

# Generate data for the second row
def generate_second_row_data():
    np.random.seed(42)
    variance = 0.5
    red_means_scaled = [(2, 2), (2, -2), (-2, 2)]
    blue_means_scaled = [(2, 0), (-2, 0), (0, -2), (0, 2)]
    red_data = np.vstack([
        np.random.multivariate_normal(mean, np.eye(2) * variance, 500 // 3) for mean in red_means_scaled
    ])
    blue_data = np.vstack([
        np.random.multivariate_normal(mean, np.eye(2) * variance, 500 // 4) for mean in blue_means_scaled
    ])
    X = np.vstack([red_data, blue_data])
    y = np.array([1] * len(red_data) + [0] * len(blue_data))
    return X, y

# Generate data for the third row
def generate_third_row_data(X_second_row, y_second_row):
    X_mean_adjusted = X_second_row - np.mean(X_second_row, axis=0)
    cov = np.cov(X_mean_adjusted, rowvar=False)
    L = np.linalg.cholesky(cov)
    X_cov_adjusted = np.dot(X_mean_adjusted, np.linalg.inv(L))
    return X_cov_adjusted, y_second_row

# Generate data for all rows
X_first_row, y_first_row = generate_first_row_data()
X_second_row, y_second_row = generate_second_row_data()
X_third_row, y_third_row = generate_third_row_data(X_second_row, y_second_row)

# Custom color map: Light Blue and Pink
cmap = ListedColormap(['lightblue', 'pink'])


# Plot decision boundaries
def plot_decision_boundary_custom_colors(clf, X, y, ax, title):
    """
    Plot decision boundary with red and blue colors and custom alpha values.
    """
    x_min, x_max = X[:, 0].min() - 1, X[:, 0].max() + 1
    y_min, y_max = X[:, 1].min() - 1, X[:, 1].max() + 1
    xx, yy = np.meshgrid(np.arange(x_min, x_max, 0.1),
                         np.arange(y_min, y_max, 0.1))
    Z = clf.predict(np.c_[xx.ravel(), yy.ravel()])
    Z = Z.reshape(xx.shape)
    ax.contourf(xx, yy, Z, alpha=0.6, cmap=cmap)
    ax.scatter(X[y == 1][:, 0], X[y == 1][:, 1], color="red", alpha=0.6, label="Class 1",s = 0.5,edgecolors=None)
    ax.scatter(X[y == 0][:, 0], X[y == 0][:, 1], color="blue", alpha=0.6, label="Class 0",s = 0.5,edgecolors=None)
    ax.set_title(title, fontsize=6, fontweight='bold', fontname='Arial')
    ax.axis('off')

# Combine all rows of data
data_rows = [(X_first_row, y_first_row), (X_second_row, y_second_row), (X_third_row, y_third_row)]

# Save the plots to a PDF file
pdf_filename = "complex_patterns_models_adjusted_size.pdf"
# Convert mm to inches for figsize (1 inch = 25.4 mm)
width_mm = 100
height_mm = 90
width_in = width_mm / 25.4
height_in = height_mm / 25.4

with PdfPages(pdf_filename) as pdf:
    fig, axes = plt.subplots(3, 5, figsize=(width_in, height_in))  # Adjust the figsize here
    fig.suptitle('Complex Patterns with Different Models (Custom Colors)', fontsize=6, fontweight='bold', fontname='Arial')

    for row_idx, (X, y) in enumerate(data_rows):
        for col_idx, (name, model) in enumerate(models.items()):
            model.fit(X, y)
            plot_decision_boundary_custom_colors(model, X, y, axes[row_idx, col_idx], f'{name}\nacc: {model.score(X, y):.2f}')

    plt.tight_layout(rect=[0, 0, 1, 0.96])
    pdf.savefig(fig)  # Save the figure to the PDF
    plt.close(fig)

print(f"PDF file saved as {pdf_filename}")

