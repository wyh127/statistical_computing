Here are several interesting theorems related to  linear regression models.



Theorem 1. Let $(X_1, X2)$ have a joint p.d.f of the form below

$f(x_1, x_2) = \frac{1}{2\pi\sigma_1\sigma_2\sqrt{1-\rho^2}}e^{-\frac{1}{2(1-\rho^2)}\{(\frac{x_1-\mu_1}{\sigma_1})^2-2\rho(\frac{x_1-\mu_1}{\sigma_1})(\frac{x_2-\mu_2}{\sigma_2})+(\frac{x_2-\mu_2}{\sigma_2})^2\}}$ 

The conditional distribution of $X_2$ given $X_1 = x_1$ is the normal distribution with mean and variance
given by

$\mathbb{E}(X_2|X_1=x_1) = \mu_1+\rho\sigma_2(\frac{x_1-\mu_1}{\sigma_1})$, $Var(X_2|X_1=x_1)=(1-\rho^2)\sigma_2^2$



Theorem 2. Suppose that $X$ and $Y$ have a joint distribution with means $\mu_X$ and $\mu_Y$ , standard deviations $\sigma_X$and $\sigma_Y$ , and correlation $\rho$. Show that if $\mathbb{E}(Y|X)$ is a linear function of $X$, then

$\mathbb{E}(Y|X) = \mu_Y+\rho\sigma_Y(\frac{X-\mu_X}{\sigma_X})$

​	


​	