import argparse
from tqdm.auto import tqdm
import numpy as np
import pandas as pd
import pickle
from datetime import datetime

import torch
from CMDN import ConvMDNPerceptron
import torch.nn as nn


# run convolutional mixture density network for input image data; tune K the number of mixture components
def main(image_data, photoz, k=7, n_train=100, n_channels=1, n_hidden=10, width=20, height=20,
         epochs=1000, lr=1e-3):
    
    galaxies = image_data['galaxies_generated']
    
    x_train = galaxies[:n_train]
    y_train = photoz[:n_train]  #alphas[:n_train]  #alphas[n_train:n_train+n_val]
    
    assert(len(x_train) == n_train)
    assert(len(y_train) == n_train)
    
    x_data = torch.from_numpy(np.array(x_train).reshape(n_train,n_channels,width,height)).float()
    y_data = torch.Tensor(y_train).float()
    
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    x_data = x_data.to(device)
    y_data = y_data.to(device)
    
    # fit the convolutional mixture density network for K=7 components
    
    # train model
    model = ConvMDNPerceptron(10, k).to(device)
    optimizer = torch.optim.Adam(model.parameters(), lr=lr)

    for epoch in range(epochs):
        optimizer.zero_grad()
        pi, mu, sigma = model(x_data)
        loss = model.loss_fn(y_data, pi, mu, sigma)
        loss.backward()
        optimizer.step()
        if epoch % (epochs / 10) == 0:
            print('Loss: ' + str(loss.item()))

    return (model)

def test_model(model, x_test, n_test, n_channels = 1, width = 20, height = 20):
    # evaluate model on test data
    x_test = x_test['galaxies_generated']
    assert(len(x_test) == n_test)
    x_test = torch.from_numpy(np.array(x_test).reshape(n_test,n_channels,width,height)).float()
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    x_test = x_test.to(device)
    pi_test, mu_test, sigma_test = model.forward(x_test)
    
    all_out_CMDN = [pi_test, mu_test, sigma_test]

    return (all_out_CMDN)