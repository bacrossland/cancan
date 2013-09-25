module CanCan
  module ModelAdapters
    class ActiveRecordAdapter < AbstractAdapter
      def self.for_class?(model_class)
        model_class <= ActiveRecord::Base
      end

      def self.override_condition_matching?(subject, name, value)
        name.kind_of?(Squeel::Nodes::Predicate) if defined? Squeel
      end

      def self.matches_condition?(subject, name, value)
        subject_value = subject.send(name.expr)
        method_name = name.method_name.to_s
        if method_name.ends_with? "_any"
          value.any? { |v| squeel_match? subject_value, method_name.sub("_any", ""), v }
        elsif method_name.ends_with? "_all"
          value.all? { |v| squeel_match? subject_value, method_name.sub("_all", ""), v }
        else
          squeel_match? subject_value, name.method_name, value
        end
      end

      def self.squeel_match?(subject_value, method, value)
        case method.to_sym
        when :eq      then subject_value == value
        when :not_eq  then subject_value != value
        when :in      then value.include?(subject_value)
        when :not_in  then !value.include?(subject_value)
        when :lt      then subject_value < value
        when :lteq    then subject_value <= value
        when :gt      then subject_value > value
        when :gteq    then subject_value >= value
        when :matches then subject_value =~ Regexp.new("^" + Regexp.escape(value).gsub("%", ".*") + "$", true)
        when :does_not_match then !squeel_match?(subject_value, :matches, value)
        else raise NotImplemented, "The #{method} Squeel condition is not supported."
        end
      end

      # Returns conditions intended to be used inside a database query. Normally you will not call this
      # method directly, but instead go through ModelAdditions#accessible_by.
      #
      # If there is only one "can" definition, a hash of conditions will be returned matching the one defined.
      #
      #   can :manage, User, :id => 1
      #   query(:manage, User).conditions # => { :id => 1 }
      #
      # If there are multiple "can" definitions, a SQL string will be returned to handle complex cases.
      #
      #   can :manage, User, :id => 1
      #   can :manage, User, :manager_id => 1
      #   cannot :manage, User, :self_managed => true
      #   query(:manage, User).conditions # => "not (self_managed = 't') AND ((manager_id = 1) OR (id = 1))"
      #
      def conditions
        if @rules.size == 1 && @rules.first.base_behavior
          # Return the conditions directly if there's just one definition
          tableized_conditions(unsqueeled_conditions(@rules.first.conditions.dup))
        else
          @rules.reverse.inject(false_sql) do |accumulator, rule|
            conditions = tableized_conditions(unsqueeled_conditions(rule.conditions.dup))
            if conditions.blank?
              rule.base_behavior ? (accumulator | true_sql) : (accumulator & false_sql)
            else
              rule.base_behavior ? (accumulator | conditions) : (accumulator & -conditions)
            end
          end
        end
      end

      def unsqueeled_conditions(conditions)
        return conditions unless conditions.kind_of? Hash
        conditions.inject({}) do |result_hash, (name, value)|
          name = name._name if name.is_a? Squeel::Nodes::Join
          if value.kind_of? Hash
            value = unsqueeled_conditions(value)
          end
          result_hash[name] = value
          result_hash
        end
      end

      def tableized_conditions(conditions, model_class = @model_class)
        return conditions unless conditions.kind_of? Hash
        conditions.inject({}) do |result_hash, (name, value)|
          if value.kind_of? Hash
            value = value.dup
            association_class = model_class.reflect_on_association(name).class_name.constantize
            nested = value.inject({}) do |nested,(k,v)|
              if v.kind_of? Hash
                value.delete(k)
                nested[k] = v
              else
                table_name = model_class.reflect_on_association(name).table_name.to_sym
                result_hash[table_name] = value
              end
              nested
            end
            result_hash.merge!(tableized_conditions(nested,association_class))
          else
            result_hash[name] = value
          end
          result_hash
        end
      end

      # Returns the associations used in conditions for the :joins option of a search.
      # See ModelAdditions#accessible_by
      def joins
        joins_hash = {}
        @rules.each do |rule|
          merge_joins(joins_hash, rule.associations_hash)
        end
        clean_joins(joins_hash) unless joins_hash.empty?
      end

      def database_records
        if override_scope
          @model_class.scoped.merge(override_scope)
        elsif @model_class.respond_to?(:where) && @model_class.respond_to?(:joins)
          mergeable_conditions = @rules.select {|rule| rule.unmergeable? }.blank?
          if mergeable_conditions
            @model_class.where(conditions).joins(joins)
          else
            @model_class.where(*(@rules.map(&:conditions))).joins(joins)
          end
        else
          @model_class.scoped(:conditions => conditions, :joins => joins)
        end
      end

      private

      def override_scope
        conditions = @rules.map(&:conditions).compact
        if defined?(ActiveRecord::Relation) && conditions.any? { |c| c.kind_of?(ActiveRecord::Relation) }
          if conditions.size == 1
            conditions.first
          else
            rule = @rules.detect { |rule| rule.conditions.kind_of?(ActiveRecord::Relation) }
            raise Error, "Unable to merge an Active Record scope with other conditions. Instead use a hash or SQL for #{rule.actions.first} #{rule.subjects.first} ability."
          end
        end
      end

      def merge_conditions(sql, conditions_hash, behavior)
        if conditions_hash.blank?
          behavior ? true_sql : false_sql
        else
          conditions = sanitize_sql(conditions_hash)
          case sql
          when true_sql
            behavior ? true_sql : "not (#{conditions})"
          when false_sql
            behavior ? conditions : false_sql
          else
            behavior ? "(#{conditions}) OR (#{sql})" : "not (#{conditions}) AND (#{sql})"
          end
        end
      end

      def false_sql
        sanitize_sql(['?=?', true, false])
      end

      def true_sql
        sanitize_sql(['?=?', true, true])
      end

      def sanitize_sql(conditions)
        @model_class.send(:sanitize_sql, conditions)
      end

      # Takes two hashes and does a deep merge.
      def merge_joins(base, add)
        add.each do |name, nested|
          if base[name].is_a?(Hash)
            merge_joins(base[name], nested) unless nested.empty?
          else
            base[name] = nested
          end
        end
      end

      # override to fix overwrites
      # do not write existing hashes using empty hashes
      def merge_joins(base, add)
        add.each do |name, nested|
          if base[name].is_a?(Hash) && nested.present?
            merge_joins(base[name], nested)
          elsif !base[name].is_a?(Hash) || nested.present?
            base[name] = nested
          end
        end
      end

      # Removes empty hashes and moves everything into arrays.
      def clean_joins(joins_hash)
        joins = []
        joins_hash.each do |name, nested|
          joins << (nested.empty? ? name : {name => clean_joins(nested)})
        end
        joins
      end
    end
  end
end

ActiveRecord::Base.class_eval do
  include CanCan::ModelAdditions
end
