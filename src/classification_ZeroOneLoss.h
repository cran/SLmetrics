#ifndef CLASSIFICATION_ZEROONELOSS_H
#define CLASSIFICATION_ZEROONELOSS_H

#include "SLmetrics.h"

namespace metric {
    template <typename T>
    class zerooneloss_score : public classification::task<T> {
    private:
        classification::confusion_matrix<T> cm_;
    
    public:
        // Unweighted constructor.
        zerooneloss_score(const vctr_t<T>& actual, const vctr_t<T>& predicted)
            : classification::task<T>(actual, predicted), cm_(actual, predicted)
        { }
    
        // Weighted constructor.
        zerooneloss_score(const vctr_t<T>& actual, const vctr_t<T>& predicted, const vctr_t<double>& weights)
            : classification::task<T>(actual, predicted, weights), cm_(actual, predicted, weights)
        { }
    
        // Rcpp::NumericMatrix constructor
        zerooneloss_score(const Rcpp::NumericMatrix& x)
            : classification::task<T>(), cm_(x)
        { }
    
        // Calculate metric
        [[nodiscard]] inline double compute() const noexcept {
            arma::Mat<double> matrix = cm_.get_matrix();
            double total = arma::accu(matrix);
            double correct = arma::accu(matrix.diag());
            
            return (total - correct) / total;
        }
    };
}

#endif
