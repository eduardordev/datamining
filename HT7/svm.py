import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sb
import sklearn.cluster as cluster
import sklearn.metrics as metrics
import sklearn.preprocessing
import pylab
import random
import math
import statsmodels.api as sm
from sklearn import linear_model
from sklearn.metrics import mean_squared_error, r2_score, accuracy_score, precision_score, recall_score, f1_score
from sklearn import datasets
from scipy import stats
from sklearn.metrics import confusion_matrix as Confusion_Matrix
from sklearn.model_selection import train_test_split
from matplotlib import pyplot
from sklearn import svm
import time
from mpl_toolkits.mplot3d import Axes3D


plt.rcParams['figure.figsize'] = (16, 9)
plt.style.use('ggplot')

#Lectura y carga de datos
train = pd.read_csv('train.csv', encoding='unicode_escape')
test = pd.read_csv('test.csv', encoding='unicode_escape')

train = train.drop(['Street', 'Id', 'MSZoning', 'Alley', 'LotShape', 'LandContour', 'Utilities', 'LotConfig', 
                         'LandSlope', 'Neighborhood', 'Condition1', 'Condition2', 'BldgType', 'HouseStyle','RoofStyle', 
                         'RoofMatl', 'Exterior1st', 'Exterior2nd', 'MasVnrType', 'ExterQual', 'ExterCond',
                         'Foundation', 'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2', 'Heating', 
                         'HeatingQC', 'CentralAir', 'Electrical', 'KitchenQual', 'Functional', 'FireplaceQu', 'GarageType',
                         'GarageFinish', 'GarageQual', 'GarageCond', 'PavedDrive','PoolQC', 'Fence', 'MiscFeature', 
                         'SaleType', 'SaleCondition'], axis=1)

train.dropna(subset = ["LotFrontage"], inplace = True)
train.dropna(subset = ["MasVnrArea"], inplace = True)
train.dropna(subset = ["GarageYrBlt"], inplace = True)

def normalizer(column):
    return (column - column.mean())/(column.std())

for col in train.columns:
    train[col] = normalizer(train[col])

    columns = []
for col in train.columns:
    columns.append(col)
    
for columna in columns:
    Q1 = train[columna].quantile(0.25)
    Q3 = train[columna].quantile(0.75)
    IQR = Q3 - Q1

    indexes = train[train[columna] < (Q1 - 1.5 * IQR)].index
    train.drop(indexes, inplace=True)

    indexes = train[train[columna] > (Q3 + 1.5 * IQR)].index
    train.drop(indexes, inplace=True)
    
    plt.boxplot(train[columna])
    plt.title(columna)
    plt.show()

#3

kmeans = cluster.KMeans(n_clusters = 3)
X = np.array(train[["SalePrice", "OverallQual", "YearBuilt", "YearRemodAdd", "TotalBsmtSF", "1stFlrSF", "GrLivArea", "FullBath", "GarageYrBlt", "GarageCars", "GarageArea"]])
X = sklearn.preprocessing.scale(X)
kmeans.fit(X)
train['KmeansCluster'] = kmeans.labels_

X = test[["OverallQual", "YearBuilt", "YearRemodAdd", "TotalBsmtSF", "1stFlrSF", "GrLivArea", "FullBath", 
                   "GarageYrBlt", "GarageCars", "GarageArea"]]
Y = test["KmeansCluster"]

#ejercicio del 4 al 6
X_train, X_test,y_train, y_test = train_test_split(X, Y,test_size=0.3,train_size=0.7)

models = (
    svm.SVC(kernel="linear", C=200),
    svm.LinearSVC(C=20, max_iter=10000),
    svm.SVC(kernel="rbf", gamma=0.3, C=15),
    svm.SVC(kernel="poly", degree=3, gamma=0.8, C=50),
    svm.SVC(kernel="sigmoid", C=50),
    svm.SVC(kernel="rbf")
)

titles = (
    "SVC with linear kernel, C = 200",
    "LinearSVC (linear kernel), C = 20",
    "SVC with RBF kernel, g = 0.3, C = 15",
    "SVC with polynomial (degree 3) kernel, g = 0.8, C = 50, d = 3",
    "SVC with sigmoid kernel, C = 20",
    "SVC with Gaussian kernel"
)

times = []
accuracies = []
errores_l = []

for i in range(0, 6):
    errores = 0
    start = time.time()
    models_r[i].fit(X_train, y_train)
    y_pred = models_r[i].predict(X_test)
    end = time.time()
    puntaje = metrics.accuracy_score(y_test, y_pred)
    print("Nombre del modelo: " + titles[i])
    print("Accuracy:",puntaje)
    print('Tiempo de ejecucion: ' + str(round((end - start), 4)))
    
    times.append(end - start)
    accuracies.append(puntaje)
    
    dat = {
        'y_test': y_test,
        'y_pred': y_pred
    }

    ev = pd.DataFrame(dat, columns=['y_test','y_pred'])

    confusion_matrix = pd.crosstab(ev['y_test'], ev['y_pred'], rownames=['y_test'], colnames=['y_pred'])

    sb.heatmap(confusion_matrix, annot=True, cmap="YlGnBu")
    plt.show()
    
    
    matriz = list(Confusion_Matrix(y_true=y_test, y_pred=y_pred))

    for i in range(0, 3):
        for j in range(0, 3):
            if i != j:
                errores += matriz[i][j]
            else:
                continue
    errores_l.append(errores)
    print('\n')


#7

d = {'Nombre': list(titles), 'Precision': accuracies, 'Tiempo de ejecucion': times, 'Errores': errores_l}
df = pd.DataFrame(data=d)
pd.options.display.max_colwidth = 100
df

#8

d2 = {'Algoritmo': ['Arbol de decisión', 'Random forest', 'Naive Bayes', 'SVM Polinomial (Menos errores obtenidos)'], 
      'Precision': [0.957, 1, 0.938, 0.991], 'Tiempo de ejecucion': [0.0137, 0.0107, 0.0515, 0.004996]}
df2 = pd.DataFrame(data=d2)
pd.options.display.max_colwidth = 100
df2