#import "template.typ": *
#show: ams-article.with(
  title: "SPICE AC Derivation",
  authors: (
    (
      name: "Keno Fischer",
      organization: [JuliaHub, Inc.],
      location: [Columbia, SC 29208],
      email: "keno@juliahub.com",
    ),
  ),
  abstract: "",
  bibliography-file: "refs.bib",
)


= AC Analysis

== A simple example

Let us begin with a concrete example, by considering a DAE corresponding to a simple RC circuit. Our state will consist entirely of voltages, one at the positive terminal of the
voltage source, the other at the positive terminal of the capacitor. Of course, only the capacitor voltage is a differential state, the other state is algebraic.

$ u = mat(
  V_C ;
  V_V
) $
$
f(u, dot(u)) = mat( dot(V_C) - frac(V_V - V_C, R C); V - V_V )
$
where $R, C, V$ are the parameters of the resistor, capacitor and voltage source respectively.

Not suppose that we are introducing a small AC perturbation in the voltage source,
i.e. we create a new DAE $g$ given by:

$
g(v, dot(v)) = f(v, dot(v)) + epsilon mat(0; A e^(i omega t))
$

For an infinitesimal perturbation $epsilon$. What can we say about the solutions $w$ to this new DAE? To find out, we take the ansatz $v(t) = u(t) + epsilon thin delta v thin e^(i omega t)$. Note that in particular, the perturbation amplitude $delta v$ has no additional time dependence (though as a complex number may of course introduce a phase) and that the derivative $dot(v)(t)$ is thus just $dot(v)(t) = dot(u)(t) + i omega epsilon thin delta v e^(i omega t)$.


Here $u(t)$ is a solution to the original DAE $f$. By taylor expanding $g$ around $u(t)$, we find

$
g(v, dot(v)) = f(u, dot(u)) + epsilon mat(0; A e^(i omega t)) + lr(frac(diff f, diff u) |)_((u, dot(u))) lr((v - u)) + lr(frac(diff f, diff dot(u)) |)_((u, dot(u))) lr((dot(v) - dot(u))) + cal(O)((v-u)^2)
$

Or expanding out or ansatz, we have:


$
g(v, dot(v)) = f(u, dot(u)) + epsilon mat(0; A e^(i omega t)) + lr(frac(diff f, diff u) |)_((u, dot(u))) epsilon thin delta v thin e^(i omega t) + i omega lr(frac(diff f, diff dot(u)) |)_((u, dot(u))) epsilon thin delta v thin e^(i omega t)
$

But recall that $u$ was a solution to $f$, so our whole DAE becomes:

$
0 = epsilon mat(0; A e^(i omega t)) + lr(frac(diff, diff u) f |)_((u, dot(u))) epsilon thin delta v thin e^(i omega t) + i omega lr(frac(diff, diff dot(u)) f |)_((u, dot(u))) epsilon thin delta v thin e^(i omega t)
$

Or, collecting like terms and dividing out the exponential and the infinitesimal:

$
lr(( lr(frac(diff, diff u) f |)_((u, dot(u))) + i omega lr(frac(diff, diff dot(u)) f |)_((u, dot(u))))) thin delta v = - mat(0; A )
$

which is a simple linear system in $delta v$ any be be readily solved:

$
delta v = - lr(( lr(frac(diff, diff u) f |)_((u, dot(u))) + i omega lr(frac(diff, diff dot(u)) f |)_((u, dot(u)))))^(-1) mat(0; A )
$

This of course generalizes to multiple sources,
which all go into the RHS.

== Generalization to arbitrary excitations

For SPICE, we are done at this point, but for our purposes, let's generalize this slightly to understand what support we need in DAECompiler.

We again consider some infinitesimally augmented system $g(v, dot(v), epsilon)$ and suppose that we have a solution $0 = g(u, dot(u), 0)$ of the unaugmented sytstem. Re-doing the exercise from above, taking $w(t) = u(t) + epsilon thin delta v(t)$, and taylor expanding to first order in epsilon, we get:

$
0 = lr(frac(diff, diff u) g |)_((u, dot(u), 0)) epsilon thin delta v (t) + lr(frac(diff, diff dot(u)) g |)_((u, dot(u), 0)) epsilon thin lr(frac(diff delta v, diff t) |)_t  + lr(frac(diff, diff epsilon) g |)_((u, dot(u), 0)) epsilon
$

Or in the AC analysis case where we take $delta v (t) arrow delta v thin e^(i omega t)$, we have
the generalization

$ delta v = - e^(i omega t) lr((lr(frac(diff, diff u) g |)_((u, dot(u), 0)) + i omega lr(frac(diff, diff dot(u)) g |)_((u, dot(u), 0))  ))  lr(frac(diff, diff epsilon) g|)_((u, dot(u), 0)) $

Note however that the LHS here is independent of time, so it probably makes more sense to consider
the system $g(w, dot(w), epsilon) = tilde(g)(w, dot(w), epsilon e^(i omega t))$ to absorb the frequency dependence.

= Noise analysis

== What does SPICE actually compute?

The first question we need to answer is what SPICE's noise analysis actually computes. It's suprisingly hard to find a precise answer here, but it when you do, it turns out that the noise analysis computes three things.

1. The "Power Spectral Density" of the noise at a particular output node. This is a function of frequency.
2. The "Equivalent Input Noise Density", defined as the Power Spectral Density divided by the gain (at the particular frequency in question) computed using standard AC analysis.
3. The "Total Integrated Noise", which is the integral of the Power Spectral Density over the frequency sweep range, often also displayed as a comulative sum.

So what is the power spectral density? Given some random variable $X(t)$, the power spectral density (at a particular frequency) is essentially defined to be the variance of the associated (via fourier transform) frequency domain random variable $tilde(X)(omega)$. However, for techincal reasons, this fourier transform does not necessarily exist, so we need a more general definition (that agrees with the moral definition when both are defined). For this definition, we consider the fourier transform $hat(X)_(T)(omega)$ of the windowed signal $X_(T)(t)$ (agreening with $X(t)$ on the interval $(-T/2, T/2)$, zero otherwise), time average and take the limit out to infinity:

$
cal(S)_X(omega) = lim_(T arrow infinity) frac(1, T) bb(E)lr([ |hat(X)_(T)(omega)|^2 ])
$

It can be shown that this is also equivalent to the fourier transform of the autocorrelation function:

$
cal(S)_X(omega) = hat(R)_(X X)(omega)
$


For a more comprehensive introduction to frequency see Demir chapter 2 and in particular section 2.2.6 for the definition of spectral densities @demirchap2.

== Noise perturbations of DAE systems

As before, we want to consider perturbations in the small signal model (this time of noise sources), so let $X_(i)(t)$ be an indexed family of stochastic processes. We will assume that stochastic process has mean zero, is stationary and has known and given spectral density $S_(X_i) (omega)$. In particular, we are not assuming that these are guassian processes, though in practice they are often modeled as such.

As before we will consider the augmented system, $g(v, dot(v), epsilon X)$ where $v(t) = u(t) + epsilon delta v (t)$ and $u(t)$ is a solution to the unaugmented system, i.e. $0 = g(u, dot(u), 0)$. However, note that in this case $delta v(t)$ is itself also a stochastic process.

Proceeding with taylor expansion as before, we find:

$
0 = lr(frac(diff, diff X) g |)_((u, dot(u), 0)) epsilon X + lr(frac(diff, diff u) g |)_((u, dot(u), 0)) epsilon thin delta v + lr(frac(diff, diff dot(u)) g |)_((u, dot(u), 0)) epsilon thin lr( frac(diff delta v, diff t) |)_t
$

Dividing by $epsilon$, multiplying by the windowing function and taking the fourier transform, we obtain

$
0 = lr(frac(diff, diff X) g |)_((u, dot(u), 0)) hat(X)_T + lr(frac(diff, diff u) g |)_((u, dot(u), 0)) hat(delta v)_T + lr(frac(diff, diff dot(u)) g |)_((u, dot(u), 0)) integral_(-T/2)^(T/2) lr( frac(diff delta v, diff t) |)_t  thin e^(-i omega t) thin d t
$

Integrating the last term by parts, we obtain

$
0 = lr(frac(diff, diff X) g |)_((u, dot(u), 0)) hat(X)_T + lr(frac(diff g, diff u) |)_((u, dot(u), 0)) hat(delta v)_T + lr(frac(diff, diff dot(u)) g |)_((u, dot(u), 0)) lr(( lr([ e^(-i omega t) frac(diff delta v, diff t) ]_(-T/2)^(T/2) + i omega hat(delta v)_T ) ))
$

Or in other words (in the large T limit):

$
hat(delta v)_T = - lr( ( lr(frac(diff g, diff u) |)_((u, dot(u), 0)) + i omega lr(frac(diff g, diff dot(u)) |)_((u, dot(u), 0))  ) )^(-1) lr(frac(diff g, diff X) |)_((u, dot(u), 0)) hat(X)_T
$

Then, letting for notational convenience

$
A = lr(frac(diff g, diff u) |)_((u, dot(u), 0)) + i omega lr(frac(diff g, diff dot(u)) |)_((u, dot(u), 0)) 
$
$
B = lr(frac(diff g, diff X) |)_((u, dot(u), 0))
$

we may write our desired PSD vector as:

$
cal(S)_(delta_v) = lim_(T arrow infinity) frac(1, T) op("diag") lr( ( A^(-1) thin B thin op("cov")(hat(X)_T) thin B^dagger (A^(-1))^dagger  ) )
$

Now, in SPICE, we are generally only interested in the PSD at a particular output node or across a particular node. Let $b$ be an arbitrary bector, where generally, $b$ might select the difference between two voltage states to compute the PSD across a branch, e.g.

$
b = mat(0; dots.v; 1; -1; dots.v; 0)
$

To find the total PSD at this branch, we write:

$
cal(S)_(delta_v dot.c b) = cal(S)_(delta_v) dot.circle b = lim_(T arrow infinity) frac(1, T) b^dagger lr( ( A^(-1) thin B thin op("cov")(hat(X)_T) thin B^dagger (A^(-1))^dagger  ) ) b
$

Under the additional assumption that the processes are independent, we thus find:

$
cal(S)_(delta_v dot.c b) = cal(S)_X dot.c |B^dagger (A^(-1))^dagger b|^2
$

Note that we could have equivalently written this as $b dot |A^(-1) B cal(S)_X|^2$, but generally the vector $b$ will be sparse, while the vector $cal(S)_X$ will not, so the expression as given is more efficient.

Note further that our matrix $X$ is the same matrix that we were using for AC analysis, except that we are now performing a solve involving the conjugate transpose of the matrix, rather than the matrix itself (though we are still using the matrix itself to compute the gain to derive the equivalent input noise).
