#ifndef CLASSIFICATION_JACCARDINDEX_H
#define CLASSIFICATION_JACCARDINDEX_H

#include "SLmetrics.h"

namespace metric {

    template <typename T>
    using base_metric = classification::metric_tools::base_metric<T>;

    using aggregate = classification::metric_tools::aggregation_level;
    
    template <typename T>
    class jaccard : public base_metric<T> {
    protected:
        // Class-wise calculation
        arma::Col<double> calculate_class_values() const override {
            // Jaccard Index = TP / (TP + FP + FN)
            return this->tp_ / (this->tp_ + this->fp_ + this->fn_);
        }
        
        // Micro average calculation
        double calculate_micro_value() const override {
            return this->calculate_micro([](double tp, double fp, double fn, double tn) {
                // Jaccard Index = TP / (TP + FP + FN)
                return tp / (tp + fp + fn);
            });
        }
        
    public:
        // Unweighted constructor
        jaccard(const vctr_t<T>& actual, 
                const vctr_t<T>& predicted, 
                aggregate mode = aggregate::CLASS_WISE, 
                bool na_rm = true) 
                : base_metric<T>(actual, predicted, mode, na_rm) {}

        // Weighted constructor
        jaccard(const vctr_t<T>& actual, 
                const vctr_t<T>& predicted,
                const vctr_t<double>& weights, 
                aggregate mode = aggregate::CLASS_WISE, 
                bool na_rm = true) 
                : base_metric<T>(actual, predicted, weights, mode, na_rm) {}

        // Matrix constructor
        jaccard(const Rcpp::NumericMatrix& x, 
                aggregate mode = aggregate::CLASS_WISE, 
                bool na_rm = true)
                : base_metric<T>(x, mode, na_rm) {}
    };
}

#endif
