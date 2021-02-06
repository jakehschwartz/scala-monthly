package com.jakehschwartz.scalamonthly

import cats.data.NonEmptyList

object Challenge {

  final case class EmployeeInfo(name: String, salesRevenue: Int, salary: Int) {
    val cost = salesRevenue - salary
  }
  sealed abstract class Employee extends Product with Serializable {
    val info: EmployeeInfo
    val cost: Int
  }
  object Employee {
    final case class Manager(info: EmployeeInfo, directReports: List[Employee]) extends Employee {
      val cost = info.cost + directReports.map(_.cost).sum
    }
    final case class IndividualContributor(info: EmployeeInfo) extends Employee {
      val cost = info.cost
    }
  }
  final case class BranchName(value: String) extends AnyVal
  final case class Branch(name: BranchName, manager: Employee.Manager)

  /**
   * To complete this challenge you will be given a list of branches. From there you will determine the profit per
   * employee of the branch and output the branch which makes the least amount of profit-per-employee so it can be shut
   * down.
   */
  def determineBranchToShutDown(branches: NonEmptyList[Branch]): BranchName = {
    branches
      .toList
      .minBy(_.manager.cost)
      .name
  }


}